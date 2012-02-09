{-# LANGUAGE TemplateHaskell, DeriveGeneric, DefaultSignatures, OverloadedStrings, FlexibleInstances #-}
module Types where
import Data.DeriveTH
import GHC.Generics
import Data.Hashable
import Data.Aeson
import Control.Applicative

--class GHashable f where
--    g_hash :: f a -> Int
    
--instance (GHashable a, GHashable b) => GHashable (a :+: b) where
--    g_hash (L)
    
import Data.Graph.Inductive

type EntityId = Node
type UserId  = EntityId
type ImageId = EntityId

type FacebookId = String

data Entity a = Entity
        {
            entity_id    :: EntityId,
            entity_value :: a
        }
        deriving(Show, Eq, Read, Generic)
        
data Rating = ThumbsUp
            | ThumbsDown
        deriving(Show, Eq, Read, Generic)
        
$(derive makeIs ''Rating)
instance ToJSON Rating
instance FromJSON Rating
        
data UserNode = UserNode
    {
        email              :: String,
        facebook_id        :: String,
        twitter_id         :: String,
        ios_device_tokens  :: [String]
    }
        deriving(Show, Eq, Read, Generic)
instance ToJSON UserNode
instance FromJSON UserNode

data ImageNode = ImageNode
    {
        image_type :: String,
        image_uri  :: String
    }      
        deriving(Show, Eq, Read, Generic)
instance ToJSON ImageNode
instance FromJSON ImageNode    
    
    
data ImageInfo = ImageInfo 
    {
        image_info_type    :: String,
        image_info_uri     :: String,
        image_info_ratings :: [Rating],
        image_info_owner   :: EntityId
    }
        deriving(Show, Eq, Read, Generic)
instance ToJSON ImageInfo
instance FromJSON ImageInfo

newtype ImageEntity = ImageEntity (Entity ImageInfo) 
                    deriving(Show, Eq, Read, Generic)
                    
data NodeType  = U UserNode
               | I ImageNode
               | R Rating
        deriving(Show, Eq, Read, Generic)
        
$(derive makeFrom ''NodeType)
$(derive makeIs ''NodeType)
instance ToJSON NodeType
instance FromJSON NodeType

        
data EdgeType  = UserCreatedImage
               | UserVotedOnImage
               | ImageInRating
        deriving(Show, Eq, Read, Generic)
instance ToJSON EdgeType
instance FromJSON EdgeType
    
$(derive makeIs ''EdgeType)
                
type EntityGraph = Gr NodeType EdgeType

instance Eq EntityGraph where
    x == y = (labNodes x == labNodes y) && (labEdges x == labEdges y)
    
instance ToJSON (Gr NodeType EdgeType) where
    toJSON x = object ["nodes" .= labNodes x, "edges" .= labEdges x]
    
instance FromJSON (Gr NodeType EdgeType) where
    parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"

type EntityContext = Context NodeType EdgeType

























 


