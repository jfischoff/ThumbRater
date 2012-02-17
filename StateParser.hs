{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module StateParser where
    
import Commands
import Types    
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import qualified Data.ByteString.Char8 as BSC
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Evaluator
import Data.Graph.Inductive
import Data.List.Split
import Data.Functor.Identity
import Data.List
import qualified Data.HashMap.Strict as H
import Control.Arrow
import Parser (parse_rating, parse_fb_id)
import Data.Tuple.Select
import Control.Monad.Trans
import Debug.Trace

{-
    I would like to describe the state easily
    
    something like
    
    images 3 1.png
           4 2.png
           
    users 1 08653456 
            images 3 
            
            ratings 5 3 ThumbsDown
            
          2 00763623
            images 4 
            ratings 5 4 ThumbsUp
                     
    New  1000 3
              4
              
    Hot  1000 4
              3
              
    fb_map 08653456 1
           00763623 2


-}

parse s = runParser parse_state () "" s

parse_state :: ParsecT String () Data.Functor.Identity.Identity  AppState
parse_state = do
    time                             <- parse_time
    image_nodes                      <- parse_images 
    (user_nodes, user_images, 
        user_ratings, rating_nodes,
        image_ratings)               <- parse_users
    fb_map                           <- parse_fb_map 
    pre_leaderboards                 <- parse_leaderboards

    
    let nodes = nub (image_nodes ++ user_nodes ++ rating_nodes)
        edges = user_images ++ user_ratings ++ image_ratings
        graph = mkGraph nodes edges
        leaderboards = H.fromList $ pre_leaderboards
    return $ AppState graph leaderboards fb_map time
    
to_image_info graph hmap = result where
    result = H.fromList $ map (second $ map (get_image_info_from_graph graph)) $ H.toList hmap
    
parse_time = do
    string "time"
    fromIntegral <$> (integer haskell)
 
parse_leaderboards :: ParsecT String u Identity [(GetImagesType, H.HashMap Int [Int])]  
parse_leaderboards = do
    string "leaderboards"
    many1 (try parse_leaderboard)

--    New  1000 3
--              4
--    
parse_leaderboard = do 
    spaces 
    typ_string <- (identifier haskell)
    spaces
    date      <- fromIntegral <$> (integer haskell)
    ids       <- many1 (try parse_id)
    return $ (read typ_string, H.singleton date ids)
    
parse_images = do
    spaces
    string "images"
    result <- many (try parse_image_node)
    return result
    
parse_image_node = do
    spaces
    node <- fromIntegral <$> (integer haskell)
    spaces 
    url <- manyTill anyChar (try space)
    let typ = last . splitOn "." $ url
    return (node, I $ ImageNode typ url)
    
parse_users = do
    spaces
    string "users"
    spaces
    results <- many1 (try parse_user_and_edges)
    return $ (\(x, y, z, w, u) -> (x, concat y, concat z, concat w, concat u)) $ unzip5 results
    
parse_user_and_edges = do
    spaces 
    node   <- fromIntegral <$> (integer haskell)
    fb_id  <- parse_fb_id
    user_images  <- do { (try (parse_user_images node)) <|> (return []) }
    (user_ratings, rating_nodes, image_ratings) <- do { try (parse_user_ratings node) <|> return ([], [], []) }
    return ((node, U $ UserNode "" fb_id "" []), user_images, user_ratings, rating_nodes, image_ratings)
 
parse_user_images user_id = do
    spaces
    string "images"
    ids <- many1 (try parse_id)
    return $ map (user_id, , UserCreatedImage) ids

parse_id = do
    spaces
    image_id      <- fromIntegral <$> (integer haskell)
    return image_id

--    fb_map 08653456 1
--           00763623 2    
parse_fb_map = do
    spaces <?> "fb space missing"
    string "fb_map" <?> "no fb_map"
    fb_and_user_ids <- many1 (try parse_fb_and_user_id)
    return $ H.fromList fb_and_user_ids

parse_fb_and_user_id = do
    spaces
    fb_id <- parse_fb_id
    spaces
    user_id <- fromIntegral <$> (integer haskell)
    return (fb_id, user_id)
    
parse_user_ratings user_id = do
    spaces
    string "ratings"
    rating_and_image_ids <- many1 (try parse_two_ids)
    
    let user_ratings  = map (user_id, , UserCreatedRating) $ map sel1 $ rating_and_image_ids
        rating_nodes  = map (\(x, y, z) -> (x, R z)) rating_and_image_ids
        image_ratings = map (\(x, y, _) -> (x, y, ImageInRating)) rating_and_image_ids
    
    return (user_ratings, rating_nodes, image_ratings)
    
parse_two_ids = do
    spaces <?> "spaces in parse two ids"
    id0 <- fromIntegral <$> (integer haskell)
    spaces 
    id1 <- fromIntegral <$> (integer haskell)
    spaces 
    rating <- parse_rating <?> "rating failed"
    spaces
    return (id0, id1, rating)
    
    

       
    
    
    