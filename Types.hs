{-# LANGUAGE TemplateHaskell, DeriveGeneric, DefaultSignatures, OverloadedStrings, 
    DeriveDataTypeable, FlexibleInstances, StandaloneDeriving, TypeFamilies, GeneralizedNewtypeDeriving,
    TypeOperators #-}
module Types where
import Data.DeriveTH
import GHC.Generics (Generic)
import Data.Hashable
import Data.Aeson
import Control.Applicative
import Data.Data
import Data.Typeable
import Data.Graph.Inductive
import Generics.Instant.TH
import qualified Generics.Instant.Functions.Show as G
import Generics.Instant.GDiff
import Generics.Instant.Base hiding (R, U)
import Normalize
import NormalizeInstances
import Data.List
import Control.Monad.Error

type EntityId = Node
type UserId  = EntityId
type ImageId = EntityId

type FacebookId = String

data Entity a = Entity
        {
            entity_id    :: EntityId,
            entity_value :: a
        }
        deriving(Show, Eq, Read, Ord, Generic, Data, Typeable)
        
instance (FromJSON a) => FromJSON (Entity a)
instance (ToJSON a)   => ToJSON (Entity a)
instance (Normalize a) => Normalize (Entity a)
        
$(deriveAll ''Entity)

instance (G.GShow a)  => G.GShow  (Entity a) where gshow      = G.gshowDefault
instance (SEq a)      => SEq      (Entity a) where shallowEq  = shallowEqDef
instance (Build a, GDiff a)    => Build    (Entity a) where build      = buildDef
instance (Children a, GDiff a) => Children (Entity a) where children   = childrenDef
instance (GDiff a)    => GDiff    (Entity a) 

-- Pairs

instance (SEq a, SEq b)      => SEq      (a, b) where shallowEq  = shallowEqDef
instance (Build a, GDiff a, Build b, GDiff b)    => Build    (a, b) where build      = buildDef
instance (Children a, GDiff a, Children b, GDiff b) => Children (a, b) where children   = childrenDef
instance (GDiff a, GDiff b)    => GDiff    (a, b)


instance Representable (a,b,c) where
  type Rep (a,b, c) = C Tuple_Triple_ ((Var a :*: Var b) :*: Var c)
  from (a,b, c)                   = C ((Var a :*: Var b) :*: Var c)
  to (C ((Var a :*: Var b) :*: Var c)) = (a,b,c)

data Tuple_Triple_
instance Constructor Tuple_Triple_ where conName _ = ",," -- Prefix?

instance (G.GShow a, G.GShow b, G.GShow c)  => G.GShow  (a, b, c) where gshow      = G.gshowDefault
instance (SEq a, SEq b, SEq c)      => SEq      (a, b, c) where shallowEq  = shallowEqDef
instance (Build a, GDiff a, Build b, GDiff b, Build c, GDiff c)    => Build    (a, b, c) where build      = buildDef
instance (Children a, GDiff a, Children b, GDiff b, Children c, GDiff c) => Children (a, b, c) where children   = childrenDef
instance (GDiff a, GDiff b, GDiff c)    => GDiff    (a, b, c)

-- Pairs
instance Representable (a,b,c,d) where
    type Rep (a,b, c, d) = C Tuple_FourTuple_ (((Var a :*: Var b) :*: Var c) :*: Var d)
    from (a,b, c, d)                   = C (((Var a :*: Var b) :*: Var c) :*: Var d)
    to (C (((Var a :*: Var b) :*: Var c) :*: Var d)) = (a,b,c,d)


data Tuple_FourTuple_
instance Constructor Tuple_FourTuple_ where conName _ = ",,," -- Prefix?

instance (G.GShow a, G.GShow b, G.GShow c, G.GShow d)  => G.GShow  (a, b, c, d) where gshow      = G.gshowDefault
instance (SEq a, SEq b, SEq c, SEq d)      => SEq      (a, b, c, d) where shallowEq  = shallowEqDef
instance (Build a, GDiff a, Build b, GDiff b, Build c, GDiff c, Build d, GDiff d)    => Build    (a, b, c, d) where build      = buildDef
instance (Children a, GDiff a, Children b, GDiff b, Children c, GDiff c, Children d, GDiff d) => Children (a, b, c, d) where children   = childrenDef
instance (GDiff a, GDiff b, GDiff c, GDiff d)    => GDiff    (a, b, c, d)
        
data Rating = ThumbsUp
            | ThumbsDown
        deriving(Show, Eq, Read, Ord, Generic, Data, Typeable)    
        
instance G.GShow  Rating where gshow      = G.gshowDefault
instance SEq      Rating where shallowEq  = shallowEqDef
instance Build    Rating where build      = buildDef
instance Children Rating where children   = childrenDef
instance GDiff Rating    
        
$(derive makeIs ''Rating)
instance ToJSON Rating
instance FromJSON Rating
instance Normalize Rating
        
$(deriveAll ''Rating)

instance Normalize [Rating] where
    normalize xs = sort $ map normalize xs
    
instance Normalize [String] where
    normalize xs = sort $ map normalize xs
        
data UserNode = UserNode
    {
        email              :: String,
        facebook_id        :: String,
        twitter_id         :: String,
        ios_device_tokens  :: [String]
    }
        deriving(Show, Eq, Read, Generic, Data, Typeable, Ord)
instance ToJSON UserNode
instance FromJSON UserNode
instance Normalize UserNode

$(deriveAll ''UserNode)   

instance G.GShow  UserNode where gshow      = G.gshowDefault
instance SEq      UserNode where shallowEq  = shallowEqDef
instance Build    UserNode where build      = buildDef
instance Children UserNode where children   = childrenDef
instance GDiff UserNode

data ImageNode = ImageNode
    {
        image_type :: String,
        image_uri  :: String
    }      
        deriving(Show, Eq, Read, Generic, Data, Typeable, Ord)
instance ToJSON ImageNode
instance FromJSON ImageNode
instance Normalize ImageNode

$(deriveAll ''ImageNode)    
instance G.GShow  ImageNode where gshow      = G.gshowDefault
instance SEq      ImageNode where shallowEq  = shallowEqDef
instance Build    ImageNode where build      = buildDef
instance Children ImageNode where children   = childrenDef
instance GDiff ImageNode
    
    
data ImageInfo = ImageInfo 
    {
        image_info_type    :: String,
        image_info_uri     :: String,
        image_info_ratings :: [Rating],
        image_info_owner   :: EntityId
    }
        deriving(Show, Eq, Read, Ord, Generic, Data, Typeable)
instance ToJSON ImageInfo
instance FromJSON ImageInfo
instance Normalize ImageInfo

$(deriveAll ''ImageInfo)

instance G.GShow  ImageInfo where gshow      = G.gshowDefault
instance SEq      ImageInfo where shallowEq  = shallowEqDef
instance Build    ImageInfo where build      = buildDef
instance Children ImageInfo where children   = childrenDef
instance GDiff ImageInfo

type ImageEntity = Entity ImageInfo
                    --deriving(Show, Eq, Read, Generic, Data, Typeable, FromJSON, ToJSON)
                    
-- $(deriveAll ''ImageEntity)
                    
data NodeType  = U UserNode
               | I ImageNode
               | R Rating
        deriving(Show, Eq, Read, Generic, Data, Typeable, Ord)
        
$(deriveAll ''NodeType)

instance G.GShow  NodeType where gshow      = G.gshowDefault
instance SEq      NodeType where shallowEq  = shallowEqDef
instance Build    NodeType where build      = buildDef
instance Children NodeType where children   = childrenDef
instance GDiff NodeType
        
fromR (R x) = return x
fromR x     = throwError ("fromR failed " ++ show x ++ " is not a Rating")

fromU (U x) = return x
fromU x     = throwError ("fromU failed " ++ show x ++ " is not a UserNode")

fromI (I x) = return x
fromI x     = throwError ("fromI failed " ++ show x ++ " is not a ImageNode")

$(derive makeIs ''NodeType)
instance ToJSON NodeType
instance FromJSON NodeType
instance Normalize NodeType

        
data EdgeType  = UserCreatedImage
               | UserCreatedRating
               | ImageInRating
        deriving(Show, Eq, Read, Generic, Data, Typeable, Ord)
        
$(deriveAll ''EdgeType)
        
instance ToJSON EdgeType
instance FromJSON EdgeType
instance Normalize EdgeType
    
$(derive makeIs ''EdgeType)
instance G.GShow  EdgeType where gshow      = G.gshowDefault
instance SEq      EdgeType where shallowEq  = shallowEqDef
instance Build    EdgeType where build      = buildDef
instance Children EdgeType where children   = childrenDef
instance GDiff EdgeType
                
type EntityGraph = Gr NodeType EdgeType

deriving instance Typeable2 Gr



instance Eq EntityGraph where
    x == y = (labNodes x == labNodes y) && (labEdges x == labEdges y)
    
instance ToJSON (Gr NodeType EdgeType) where
    toJSON x = object ["nodes" .= labNodes x, "edges" .= labEdges x]
    
instance FromJSON (Gr NodeType EdgeType) where
    parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"

type EntityContext = Context NodeType EdgeType




























 


