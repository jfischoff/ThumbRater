{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, StandaloneDeriving,
    DeriveGeneric, DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances,
    FlexibleInstances, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, 
    MultiParamTypeClasses, UndecidableInstances #-}
module Evaluator where
import Control.Applicative
import PushApple.AppContext
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans
import Types
import Commands
import Control.Arrow
import Control.Monad.Identity
import Data.Maybe
import qualified Data.HashMap.Strict as H
import Data.Lens.Strict
import Data.Lens.Template
import Data.Graph.Inductive
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Tuple.Select
import System.Time
import Facebook
import Data.Aeson
import GHC.Generics
import Control.Arrow
import qualified Data.Text as T
import Generics.Pointless.MonadCombinators
import Data.Hashable
import Debug.Trace.Helpers
import Data.Data
import Normalize
import Data.Typeable
import Control.Monad.Error
import ErrorTest

--TODO
--Get the evaluator tester working again
--Write Unit Tests
--Write queckchecks



type ImageLeaderboards = H.HashMap GetImagesType (H.HashMap Date [EntityId])

instance Normalize [EntityId] where
    normalize xs = sort $ map normalize xs
    
instance Normalize [(EdgeType, Node)] where
    normalize xs = sort $ map normalize xs

instance Normalize  [(Date, [EntityId])] where
    normalize xs = sort $ map normalize xs

instance Normalize  [(String, EntityId)] where
    normalize xs = sort $ map normalize xs

instance Normalize [(GetImagesType, [(Date, [EntityId])])] where
    normalize xs = sort $ map normalize xs
   
cxt_node (_, i, _, _) = i 
instance Normalize [Context NodeType EdgeType] where
    normalize = sortBy (\x y -> cxt_node y `compare` cxt_node x) . map normalize

instance Normalize ImageLeaderboards where
    normalize x = H.fromList $ map (second (H.fromList . map (second normalize) . H.toList)) $ H.toList x

instance Normalize (H.HashMap String Int) where
    normalize = id

instance (Show a, ToJSON b) => ToJSON (H.HashMap a b) where
    toJSON = object . map (T.pack . show *** toJSON ) . H.toList
    
instance (Read a, Eq a, Hashable a, FromJSON b) => FromJSON (H.HashMap a b) where
    parseJSON (Object v) = do
         let list = H.toList v 
         pairs <- mapM (\(x, y) -> mstrength (read $ T.unpack x, parseJSON y)) list
         return $ H.fromList pairs

data AppReadonly = AppReadonly 
    {
        _state_filepath :: FilePath,
        _should_save    :: Bool
    }
    deriving(Show, Eq, Data, Typeable)

$(makeLens ''AppReadonly)
    
data AppState = AppState
    {
        _graph :: EntityGraph,
        _leaderboards           :: ImageLeaderboards,
        _facebook_id_to_user_id  :: H.HashMap String Int,
        _time                    :: Int
    }
    deriving(Show, Eq, Generic)
    
instance Normalize AppState    
instance ToJSON AppState
instance FromJSON AppState

$(makeLens ''AppState)
{-
data InternalError = LeaderboardNotFound GetImagesType
                   | DateNotFound Date
                   | KeyNotFound String
                   | IndexNotFound Int
                   | SomethingBad String
                   deriving(Show, Eq)

instance Error InternalError where
    noMsg    = SomethingBad "Panic!"
    strMsg s = SomethingBad s
-}

type InternalError = String





type EvalState d = SuperStateT AppReadonly () AppState InternalError IO d


run_context' default_state default_config commands = runSuperState commands default_config default_state
    

run_evaluator x y input = run_context' y x (eval input)

load :: EvalState ()
load = do
    filepath <- asks (state_filepath^$)
    !json    <- liftIO $ BSL.readFile filepath
    s        <- un_maybe "json decoding failed" $ decode json
    put s

persist = do
    filepath <- asks (state_filepath^$)
    json <- gets encode
    b <- asks _should_save
    if b
            then liftIO $ BSL.writeFile filepath json
            else return ()
    
eval :: Input -> EvalState Output
eval x = do 
    output <- eval' x
    persist
    return output
    
eval' :: Input -> EvalState Output
eval' (InputLogin     x) = OutputLogin     <$> login x 
eval' (InputRate      x) = OutputRate      <$> rate x
eval' (InputUpload    x) = OutputUpload    <$> upload x
eval' (InputGetImages x) = OutputGetImages <$> get_images x
eval' (Print State)         = do 
    s <- get
    liftIO $ print s
    return Empty
eval' (Print Leaderboards)         = do 
    s <- gets (leaderboards^$)
    liftIO $ print s
    return Empty


login :: LoginInput -> EvalState LoginOutput
login x = do
    let fb_id = login_input_facebook_id x
    exists <- user_exists_with_facebook fb_id
    if exists
        then login_existing_user_with_facebook fb_id
        else do 
                create_user x
                ask_facebook_permissions fb_id
                login_existing_user_with_facebook fb_id
                
user_exists_with_facebook facebook_id = gets (isJust . H.lookup facebook_id . (facebook_id_to_user_id^$))
login_existing_user_with_facebook facebook_id = return $ LoginOutput ""
create_user x = do
    new_id <- allocate_id
    let new_user_node = UserNode "" (login_input_facebook_id x) "" []
    add_context ([], new_id, U new_user_node, [])
    update_facebook_to_id_cache (login_input_facebook_id x) new_id

update_facebook_to_id_cache facebook_id new_id = do
    modify (facebook_id_to_user_id ^%= H.insert facebook_id new_id)
    
ask_facebook_permissions x = liftIO $ print "asking facebook"
                
rate :: RateInput -> EvalState RateOutput
rate x = do
    let image_id = image_rating_image_id . user_input . fromRateInput $ x
    add_rating (get_user_id x) image_id
                        (image_rating_rating . user_input . fromRateInput $ x)
                        
    recompute_leaderboard Hot
    user_node <- get_image_owner_node image_id
    send_rated_notification image_id user_node
    return RateOutput
  
  
--send a notification of changes  
recompute_leaderboard Hot = do
    image_infos <- gets (collect_image_infos . (graph^$))
    sorted_infos <- liftM sort_images_by_ratings image_infos
    add_leaderboard Hot $ map entity_id sorted_infos
 
update_leaderboard :: GetImagesType -> EntityId -> EvalState ()
update_leaderboard New image_id = do
    now <- get_now
    let day = (now `div` (24 * 60 * 60)) * (24 * 60 * 60)
    modify (leaderboards ^%= update_or_add_leaderboard New day [image_id] )
                 
update_or_add_leaderboard typ day image_infos = result where
    result = H.insertWith ((uncurry (H.insertWith (++))) . head . H.toList) typ 
                 $ H.singleton day image_infos
    
add_leaderboard :: GetImagesType -> [EntityId] -> EvalState ()
add_leaderboard typ new = do
    now <- get_now
    add_leaderboard' now typ new
    
add_leaderboard' date typ new = do
    modify (leaderboards ^%= add_leaderboard_to_hashmap typ date new)
    
add_leaderboard_to_hashmap typ date new = H.insertWith H.union typ $ H.singleton date new
    
get_now = gets (time^$)

get_image_info :: EntityId -> EvalState ImageEntity
get_image_info image_id = do 
    cxt <- gets ((flip context) image_id . (graph^$))
    mk_image_info_from_cxt' cxt
    
get_image_info_from_graph graph image_id = result where
    cxt = context graph image_id
    result = mk_image_info_from_cxt graph cxt
  
--collect_image_ids :: EntityGraph -> [EntityId]
collect_image_ids = map node' . filter (isI . lab') . decompose_graph 
    
collect_image_infos :: EntityGraph -> EvalState [ImageEntity]
collect_image_infos graph = result where
    result = mapM (mk_image_info_from_cxt graph) . filter (isI . lab') $ decompose_graph graph
 
--mk_image_info_from_cxt' :: EntityContext -> EvalState ImageEntity
mk_image_info_from_cxt' cxt = do 
    gr <- gets (graph^$)
    mk_image_info_from_cxt gr cxt 
 
--mk_image_info_from_cxt :: (MonadError InternalError m) => EntityGraph -> EntityContext -> m ImageEntity  
mk_image_info_from_cxt graph (ins, image_id, I node_info, outs) = do 
    let rating_ids = map snd $ filter (isImageInRating . fst) (ins ++ outs)
    ratings    <- mapM (\x -> fromR =<< (un_maybe "rating not found" $ lab graph x)) rating_ids
    owner_id   <- un_maybe "owner id not found" $ lookup UserCreatedImage ins
    return $ Entity image_id $ ImageInfo (image_type node_info) (image_uri node_info) ratings owner_id
    
sort_images_by_ratings image_infos = result where
    result = sortBy (\x y -> (score_image $ entity_value y) `compare` (score_image $ entity_value x)) image_infos
    
score_image info = result where
    result = (toRational thumbs_up_count) / (toRational (total_ratings + 2))
    thumbs_up_count = length $ filter isThumbsUp $ image_info_ratings info
    total_ratings = length $ image_info_ratings info
    
    
send_rated_notification image_id user_node = do
    email_rating_changed image_id (email user_node)
    mapM_ (iOS_rated_notification image_id) (ios_device_tokens user_node)
    --facebook notifications

email_rating_changed image_id email = do
    let message = "should email to " ++ show email ++ " that " ++ show image_id ++ "changed"
    liftIO $ print message
    
iOS_rated_notification image_id device_token = do
    let message = "should notify the device " ++ show device_token ++ "that the image" ++ show image_id ++ "changed"
    liftIO $ print message
    
upload :: UploadInput -> EvalState UploadOutput
upload x = do
     url <- create_image_url
     save_image url (image_data_bytes . user_input $ fromUploadInput x)
     image_id <- add_image (get_user_id x) url (image_data_type . user_input $ fromUploadInput x)
     update_leaderboard New image_id
     recompute_leaderboard Hot
     return UploadOutput

create_image_url = do
    num <- gets (noNodes . (graph^$))
    return $ (show (num + 1)) ++ ".png"
    
save_image url bytes = liftIO $ BS.writeFile url bytes

get_images :: GetImagesInput -> EvalState GetImagesOutput
get_images x = get_images_type (get_images_input_type x) x

get_leaderboard' :: GetImagesType -> EvalState (H.HashMap Date [EntityId])
get_leaderboard' typ = do
    un_maybe "leaderboard type not found" =<< gets (H.lookup typ . (leaderboards ^$))  

has_leaderboard typ date = isJust <$> H.lookup date <$> get_leaderboard' typ

get_leaderboard :: GetImagesType -> Date -> EvalState [EntityId]
get_leaderboard typ date = un_maybe ("leaderboard date not found " ++ (show date))=<< (H.lookup date) <$> get_leaderboard' typ
 
get_ids_in_range start count leader_board = take count $ snd $ splitAt start leader_board
    
get_leader_board_infos typ date start count = do
    leader_board <- get_leaderboard typ date
    let ids = get_ids_in_range count start leader_board
    infos <- mapM get_image_info ids
    return $ GetImagesOutput infos
    
get_images_type :: GetImagesType -> GetImagesInput -> EvalState GetImagesOutput
get_images_type Hot x = get_leader_board_infos Hot (get_images_input_date x) (get_images_input_count x) 
     (get_images_input_start x)
get_images_type New x = get_leader_board_infos New (get_images_input_date x) (get_images_input_count x) 
          (get_images_input_start x)

--Send emails
allocate_id = gets (head . newNodes 1 . (graph^$)) 
add_context context = modify (graph ^%= (&) context)

add_rating user_id image_id rating = do
    new_id <- allocate_id
    let user_created_edge   = (UserCreatedRating, user_id)
        image_voted_on_edge = (ImageInRating, image_id)
        
        context = ([user_created_edge], new_id, R rating, [image_voted_on_edge])
    add_context context
    
add_image user_id url typ = do 
    new_id <- allocate_id
    let user_created_edge = (UserCreatedImage, user_id)
        context = ([user_created_edge], new_id, I $ ImageNode typ url, [])
    add_context context
    return new_id
    
get_image_owner_node image_id = do
    owner_id  <- un_maybe "no created edge found" =<< gets (lookup UserCreatedImage . sel1 . (flip context) image_id . (graph^$)) 
    node      <- un_maybe "owner node not found" =<< gets ((flip lab) owner_id . (graph^$))    
    fromU node

decompose_graph graph = result where
    result = map (context graph) ns
    ns = nodes graph 
    
instance Normalize (Gr NodeType EdgeType) where
    normalize x = foldl' (flip (&)) Data.Graph.Inductive.empty $ normalize $ decompose_graph x


un_maybe err (Just x) = return x
un_maybe err x        = throwError err


