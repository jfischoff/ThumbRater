{-#  LANGUAGE GADTs  #-}
{-#  LANGUAGE KindSignatures  #-}
{-#  LANGUAGE TypeFamilies  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE RankNTypes  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE OverlappingInstances  #-} --  Only for the Show
{-#  LANGUAGE NoMonomorphismRestriction  #-} --  Only for the Show

module StateDiff where
import Data.Generic.Diff
import Commands
import Types
import Data.Graph.Inductive hiding (empty)
import System.Console.ANSI ( Color(Green, Red, Black), ColorIntensity(Dull)
                           , ConsoleLayer(Foreground), SGR(SetColor, Reset)
                           , setSGRCode )
import Text.PrettyPrint.Free
import Control.Monad.State

type AppStateTuple =  (ContextList,
                       ImagesTypePairList,
                       FBIdPairList,
                       Int)

type StringList   = [String]
type ContextAdj   = (EdgeType, Node)
type AdjList      = [ContextAdj]
type GraphContext = Context NodeType EdgeType
type ContextList  = [GraphContext]
type RatingList   = [Rating]


type EntityIdList   = [EntityId]
type DateEntityListPair = (Date, EntityIdList)
type DateEntityListList = [DateEntityListPair]
type ImagesTypePair     = (GetImagesType, DateEntityListList)
type ImagesTypePairList = [ImagesTypePair]
type FBIdPair           = (String, Int)
type FBIdPairList       = [FBIdPair]


data AppStateTupleFam :: * -> * -> * where
    String'                :: String        -> AppStateTupleFam String Nil
    Int'                   :: Int           -> AppStateTupleFam Int Nil
    New'                   :: AppStateTupleFam GetImagesType Nil
    Hot'                   :: AppStateTupleFam GetImagesType Nil
    UserCreatedImage'      :: AppStateTupleFam EdgeType Nil
    UserCreatedRating'     :: AppStateTupleFam EdgeType Nil
    ImageInRating'         :: AppStateTupleFam EdgeType Nil    
    U'                     :: AppStateTupleFam NodeType (Cons UserNode Nil)
    I'                     :: AppStateTupleFam NodeType (Cons ImageNode Nil)
    R'                     :: AppStateTupleFam NodeType (Cons Rating Nil)
    ThumbsUp'              :: AppStateTupleFam Rating Nil
    ThumbsDown'            :: AppStateTupleFam Rating Nil
    NilStringList'         :: AppStateTupleFam StringList Nil
    StringList'            :: AppStateTupleFam StringList (Cons String (Cons StringList Nil))
    UserNode'              :: AppStateTupleFam UserNode (Cons String (Cons String 
                                    (Cons String (Cons StringList Nil))))
    ImageNode'             :: AppStateTupleFam ImageNode (Cons String (Cons String Nil))
    NilRatingList'         :: AppStateTupleFam RatingList Nil
    RatingList'            :: AppStateTupleFam RatingList (Cons Rating (Cons RatingList Nil))
    ImageInfo'             :: AppStateTupleFam ImageInfo (Cons String (Cons String (Cons RatingList (Cons Int Nil))))
    ImageEntity'           :: AppStateTupleFam ImageEntity (Cons Int (Cons ImageInfo Nil))
    ContextAdj'            :: AppStateTupleFam ContextAdj (Cons EdgeType (Cons Node Nil))
    NilAdjList'            :: AppStateTupleFam AdjList Nil
    AdjList'               :: AppStateTupleFam AdjList (Cons ContextAdj (Cons AdjList Nil))
    GraphContext'          :: AppStateTupleFam GraphContext (Cons AdjList (Cons Node (Cons NodeType (Cons AdjList Nil))))
    NilContextList'        :: AppStateTupleFam ContextList Nil
    ContextList'           :: AppStateTupleFam ContextList (Cons GraphContext (Cons ContextList Nil))
    FBIdPair'              :: AppStateTupleFam FBIdPair (Cons String (Cons Int Nil))
    NilFBIdPairList'        :: AppStateTupleFam FBIdPairList Nil
    FBIdPairList'          :: AppStateTupleFam FBIdPairList (Cons FBIdPair (Cons FBIdPairList Nil))
    NilEntityIdList'       :: AppStateTupleFam EntityIdList Nil
    EntityIdList'          :: AppStateTupleFam EntityIdList (Cons EntityId (Cons EntityIdList Nil))
    DateEntityListPair'    :: AppStateTupleFam DateEntityListPair (Cons Date (Cons EntityIdList Nil))
    NilDateEntityListList'  :: AppStateTupleFam DateEntityListList Nil
    DateEntityListList'    :: AppStateTupleFam DateEntityListList (Cons DateEntityListPair (Cons DateEntityListList Nil))
    ImagesTypePair'        :: AppStateTupleFam ImagesTypePair (Cons GetImagesType (Cons DateEntityListList Nil))
    NilImagesTypePairList'  :: AppStateTupleFam ImagesTypePairList Nil
    ImagesTypePairList'     :: AppStateTupleFam ImagesTypePairList (Cons ImagesTypePair (Cons ImagesTypePairList Nil))
    AppStateTuple'          :: AppStateTupleFam AppStateTuple (Cons ContextList (Cons ImagesTypePairList (Cons FBIdPairList (Cons Int Nil))))
    
instance Family AppStateTupleFam where
    decEq (String' x) (String' y) | x == y = Just (Refl, Refl)
                                  | otherwise = Nothing
    decEq (Int' x) (Int' y)       | x == y = Just (Refl, Refl)    
                                  | otherwise = Nothing    
    decEq New' New'                                     = Just (Refl, Refl)
    decEq Hot' Hot'                                     = Just (Refl, Refl)            
    decEq UserCreatedImage'      UserCreatedImage'      = Just (Refl, Refl)
    decEq UserCreatedRating'     UserCreatedRating'     = Just (Refl, Refl)
    decEq ImageInRating'         ImageInRating'         = Just (Refl, Refl)
    decEq U'                     U'                     = Just (Refl, Refl)
    decEq I'                     I'                     = Just (Refl, Refl)
    decEq R'                     R'                     = Just (Refl, Refl)
    decEq ThumbsUp'              ThumbsUp'              = Just (Refl, Refl) 
    decEq ThumbsDown'            ThumbsDown'            = Just (Refl, Refl) 
    decEq NilStringList'         NilStringList'         = Just (Refl, Refl) 
    decEq StringList'            StringList'            = Just (Refl, Refl) 
    decEq UserNode'              UserNode'              = Just (Refl, Refl)                                   
    decEq ImageNode'             ImageNode'             = Just (Refl, Refl) 
    decEq NilRatingList'         NilRatingList'         = Just (Refl, Refl) 
    decEq RatingList'            RatingList'            = Just (Refl, Refl) 
    decEq ImageInfo'             ImageInfo'             = Just (Refl, Refl)
    decEq ImageEntity'           ImageEntity'           = Just (Refl, Refl) 
    decEq ContextAdj'            ContextAdj'            = Just (Refl, Refl)
    decEq NilAdjList'            NilAdjList'            = Just (Refl, Refl)
    decEq AdjList'               AdjList'               = Just (Refl, Refl)
    decEq GraphContext'          GraphContext'          = Just (Refl, Refl)
    decEq NilContextList'        NilContextList'        = Just (Refl, Refl)
    decEq ContextList'           ContextList'           = Just (Refl, Refl)
    decEq FBIdPair'              FBIdPair'              = Just (Refl, Refl) 
    decEq NilFBIdPairList'       NilFBIdPairList'        = Just (Refl, Refl) 
    decEq FBIdPairList'          FBIdPairList'          = Just (Refl, Refl) 
    decEq NilEntityIdList'       NilEntityIdList'       = Just (Refl, Refl) 
    decEq EntityIdList'          EntityIdList'          = Just (Refl, Refl) 
    decEq DateEntityListPair'    DateEntityListPair'    = Just (Refl, Refl) 
    decEq NilDateEntityListList' NilDateEntityListList'  = Just (Refl, Refl) 
    decEq DateEntityListList'    DateEntityListList'    = Just (Refl, Refl) 
    decEq ImagesTypePair'        ImagesTypePair'        = Just (Refl, Refl) 
    decEq NilImagesTypePairList' NilImagesTypePairList' = Just (Refl, Refl)  
    decEq ImagesTypePairList'    ImagesTypePairList'    = Just (Refl, Refl)  
    decEq AppStateTuple'         AppStateTuple'         = Just (Refl, Refl) 
    decEq _ _ = Nothing
    
    fields (String' _)            _                      = Just CNil
    fields (Int' _)               _                      = Just CNil
    fields New'                   New                    = Just CNil
    fields Hot'                   Hot                    = Just CNil
    fields UserCreatedImage'      UserCreatedImage       = Just CNil
    fields UserCreatedRating'     UserCreatedRating      = Just CNil
    fields ImageInRating'         ImageInRating          = Just CNil
    fields U'                     (U x)                  = Just (CCons x CNil)
    fields I'                     (I x)                  = Just (CCons x CNil)
    fields R'                     (R x)                  = Just (CCons x CNil)
    fields ThumbsUp'              ThumbsUp               = Just CNil
    fields ThumbsDown'            ThumbsDown             = Just CNil
    fields NilStringList'         []                     = Just CNil
    fields StringList'            (x:xs)                 = Just (CCons x (CCons xs CNil))  
    fields UserNode'              (UserNode x y z w)     = Just (CCons x (CCons y (CCons z (CCons w CNil)))) 
    fields ImageNode'             (ImageNode x y)        = Just (CCons x (CCons y CNil)) 
    fields NilRatingList'         []                     = Just CNil
    fields RatingList'            (x:xs)                 = Just (CCons x (CCons xs CNil))  
    fields ImageInfo'             (ImageInfo x y z w)   = Just (CCons x (CCons y (CCons z (CCons w CNil)))) 
    fields ImageEntity'           (Entity x y)          = Just (CCons x (CCons y CNil)) 
    fields ContextAdj'            (x, y)                = Just (CCons x (CCons y CNil)) 
    fields NilAdjList'            []                    = Just CNil
    fields AdjList'               (x:xs)                = Just (CCons x (CCons xs CNil))  
    fields GraphContext'          (x,y,z,w)             = Just (CCons x (CCons y (CCons z (CCons w CNil)))) 
    fields NilContextList'        []                    = Just CNil
    fields ContextList'           (x:xs)                = Just (CCons x (CCons xs CNil))  
    fields FBIdPair'              (x,y)                 = Just (CCons x (CCons y CNil)) 
    fields NilFBIdPairList'        []                    = Just CNil
    fields FBIdPairList'          (x:xs)                = Just (CCons x (CCons xs CNil))  
    fields NilEntityIdList'       []                    = Just CNil
    fields EntityIdList'          (x:xs)                = Just (CCons x (CCons xs CNil))  
    fields DateEntityListPair'    (x, y)                = Just (CCons x (CCons y CNil)) 
    fields NilDateEntityListList'  []                    = Just CNil
    fields DateEntityListList'    (x:xs)                = Just  (CCons x (CCons xs CNil))  
    fields ImagesTypePair'        (x,y)                 = Just (CCons x (CCons y CNil)) 
    fields NilImagesTypePairList'  []                   = Just CNil
    fields ImagesTypePairList'     (x:xs)               = Just (CCons x (CCons xs CNil))  
    fields AppStateTuple'          (x, y, z, w)         = Just (CCons x (CCons y (CCons z (CCons w CNil)))) 
    fields _ _ = Nothing

    apply (String' s)            CNil                                         = s
    apply (Int' i)               CNil                                         = i
    apply New'                   CNil                                         = New                    
    apply Hot'                   CNil                                         = Hot                    
    apply UserCreatedImage'      CNil                                         = UserCreatedImage       
    apply UserCreatedRating'     CNil                                         = UserCreatedRating      
    apply ImageInRating'         CNil                                         = ImageInRating          
    apply U'                     (CCons x CNil)                               = (U x)                  
    apply I'                     (CCons x CNil)                               = (I x)                  
    apply R'                     (CCons x CNil)                               = (R x)                  
    apply ThumbsUp'              CNil                                         = ThumbsUp               
    apply ThumbsDown'            CNil                                         = ThumbsDown             
    apply NilStringList'         CNil                                         = []                     
    apply StringList'            (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply UserNode'              (CCons x (CCons y (CCons z (CCons w CNil)))) = (UserNode x y z w)     
    apply ImageNode'             (CCons x (CCons y CNil))                     = (ImageNode x y)        
    apply NilRatingList'         CNil                                         = []                     
    apply RatingList'            (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply ImageInfo'             (CCons x (CCons y (CCons z (CCons w CNil)))) = (ImageInfo x y z w)    
    apply ImageEntity'           (CCons x (CCons y CNil))                     = (Entity x y)           
    apply ContextAdj'            (CCons x (CCons y CNil))                     = (x, y)                 
    apply NilAdjList'            CNil                                         = []                     
    apply AdjList'               (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply GraphContext'          (CCons x (CCons y (CCons z (CCons w CNil)))) = (x,y,z,w)              
    apply NilContextList'        CNil                                         = []                     
    apply ContextList'           (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply FBIdPair'              (CCons x (CCons y CNil))                     = (x,y)                  
    apply NilFBIdPairList'       CNil                                         =  []                    
    apply FBIdPairList'          (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply NilEntityIdList'       CNil                                         = []                     
    apply EntityIdList'          (CCons x (CCons xs CNil))                    = (x:xs)                 
    apply DateEntityListPair'    (CCons x (CCons y CNil))                     = (x, y)                 
    apply NilDateEntityListList' CNil                                         =  []                    
    apply DateEntityListList'     (CCons x (CCons xs CNil))                   =  (x:xs)                
    apply ImagesTypePair'        (CCons x (CCons y CNil))                     =  (x,y)                 
    apply NilImagesTypePairList' CNil                                         =  []                    
    apply ImagesTypePairList'    (CCons x (CCons xs CNil))                    =  (x:xs)                
    apply AppStateTuple'         (CCons x (CCons y (CCons z (CCons w CNil)))) =  (x, y, z, w)          
    
    string (String' s)            = show s
    string (Int' i)               = show i
    string New'                   = "New"
    string Hot'                   = "Hot"
    string UserCreatedImage'      = "UserCreatedImage"
    string UserCreatedRating'     = "UserCreatedRating"
    string ImageInRating'         = "ImageInRating"
    string U'                     = "U" 
    string I'                     = "I"
    string R'                     = "R"
    string ThumbsUp'              = "ThumbsUp"
    string ThumbsDown'            = "ThumbsDown"
    string NilStringList'         = "[]"
    string StringList'            = "StringList"
    string UserNode'              = "UserNode"
    string ImageNode'             = "ImageNode" 
    string NilRatingList'         = "[]"
    string RatingList'            = "RatingList"
    string ImageInfo'             = "ImageInfo"
    string ImageEntity'           = "ImageEntity"
    string ContextAdj'            = "ContextAdj"
    string NilAdjList'            = "[]"
    string AdjList'               = "AdjList"
    string GraphContext'          = "GraphContext"
    string NilContextList'        = "[]"
    string ContextList'           = "ContextList"
    string FBIdPair'              = "FBIdPair"
    string NilFBIdPairList'       = "[]"
    string FBIdPairList'          = "FBIdPairList"
    string NilEntityIdList'       = "[]"
    string EntityIdList'          = "EntityIdList"
    string DateEntityListPair'    = "DateEntityListPair"
    string NilDateEntityListList' = "[]"
    string DateEntityListList'    = "DateEntityListList"
    string ImagesTypePair'        = "ImagesTypePair"
    string NilImagesTypePairList' = "[]"
    string ImagesTypePairList'    = "ImagesTypePairList"
    string AppStateTuple'         = "AppStateTuple"
    
instance Type AppStateTupleFam Int where
    constructors = [Abstr Int']

instance Type AppStateTupleFam String where
    constructors = [Abstr String']
    
instance Type AppStateTupleFam GetImagesType where
    constructors = [Concr Hot', Concr New']    
    
instance Type AppStateTupleFam EdgeType where
    constructors = [Concr UserCreatedImage', Concr UserCreatedRating', Concr ImageInRating']

instance Type AppStateTupleFam ImageNode where
    constructors = [Concr ImageNode']
    
instance Type AppStateTupleFam Rating where
    constructors = [Concr ThumbsDown', Concr ThumbsUp']

instance Type AppStateTupleFam UserNode where
    constructors = [Concr UserNode']

instance Type AppStateTupleFam StringList where
    constructors = [Concr StringList', Concr NilStringList']

instance Type AppStateTupleFam NodeType where
    constructors = [Concr U', Concr I', Concr R']
    
instance Type AppStateTupleFam RatingList where
    constructors = [Concr RatingList', Concr NilRatingList']
    
instance Type AppStateTupleFam ImageInfo where
    constructors = [Concr ImageInfo']
        
instance Type AppStateTupleFam ImageEntity where
    constructors = [Concr ImageEntity']

instance Type AppStateTupleFam ContextAdj where
    constructors = [Concr ContextAdj']

instance Type AppStateTupleFam AdjList where
    constructors = [Concr AdjList', Concr NilAdjList']
    
instance Type AppStateTupleFam GraphContext where
    constructors = [Concr GraphContext']

instance Type AppStateTupleFam ContextList where
    constructors = [Concr ContextList', Concr NilContextList']

instance Type AppStateTupleFam FBIdPair where
    constructors = [Concr FBIdPair']

instance Type AppStateTupleFam FBIdPairList where
    constructors = [Concr FBIdPairList', Concr NilFBIdPairList']

instance Type AppStateTupleFam EntityIdList where
    constructors = [Concr EntityIdList', Concr NilEntityIdList']

instance Type AppStateTupleFam DateEntityListPair where
    constructors = [Concr DateEntityListPair']

instance Type AppStateTupleFam DateEntityListList where
    constructors = [Concr DateEntityListList', Concr NilDateEntityListList']

instance Type AppStateTupleFam ImagesTypePair where
    constructors = [Concr ImagesTypePair']

instance Type AppStateTupleFam ImagesTypePairList where
    constructors = [Concr ImagesTypePairList', Concr NilImagesTypePairList']

instance Type AppStateTupleFam AppStateTuple where
    constructors = [Concr AppStateTuple']
    


color c = text (setSGRCode [SetColor Foreground Dull c])
black = color Black
red   = color Red
green = color Green

fmt x@(Cpy c d) = fst $ runState (fmt_diff c x) 0

type IdentState = State Int

g_diff l col c d    = do
    y <- fmt_diff c d
    x <- fmt_c l c y
    i <- get
    return ((indent i col) <+> x)

--can do all the color checking here
fmt_diff :: AppStateTupleFam t ts -> EditScriptL AppStateTupleFam txs tys -> IdentState (Doc e)
fmt_diff l (Cpy c d)    = g_diff l black c d
fmt_diff l (Del c d)    = g_diff l red c d
fmt_diff l (Ins c d)    = g_diff l green c d
fmt_diff l (CpyTree d)  = do 
    x <- fmt_diff l d
    return ((align black) <+> x)
fmt_diff l End          = return Text.PrettyPrint.Free.empty
    
fmt_c :: AppStateTupleFam r rs -> AppStateTupleFam t ts -> Doc e -> IdentState (Doc e)
fmt_c AppStateTuple' AppStateTuple' x = return x
fmt_c _              ContextList'   x = do 
    dec
    return $ (text "ContextList" </> x)
fmt_c _  GraphContext'  x = do 
    dec
    return $ (text "(" </> x)
fmt_c _ AdjList' x = do
    dec 
    return $ (text "[" </> x)
fmt_c ContextAdj' NilAdjList' x = do
    inc 
    return $ (text "]" </> x)
fmt_c GraphContext' NilAdjList' x = return $ (text "[]," </> x)
fmt_c _ (Int' i) x = return $ (pretty i <> text "," </> x)
fmt_c _ U' x = return x
fmt_c _ UserNode' x = do 
    dec
    return x
fmt_c _ (String' s) x = return $ text (show s) </> x
fmt_c (String' s) NilStringList' x = do
    inc
    return $ text "[]" </> x

fmt_c l c x = return $ ((indent 0 $ text $ string c) </> x)


inc = do
    modify (1+)
    
    
dec = do
    modify (1-)

    
    
    
    
    
    
    
    
    
    


