{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, 
    DeriveDataTypeable, TypeFamilies, DeriveGeneric,
    TypeOperators #-}
module Commands where
import Data.DeriveTH
import Types
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Aeson
import Data.Data
import Data.Typeable
import Generics.Instant.TH
import qualified Generics.Instant.Functions.Show as G
import Generics.Instant.GDiff
import Generics.Instant.Base hiding (R, U)
import Normalize
import GHC.Generics
import Data.List

type FacebookSessionInfo = String
type FacebookSignupError = String


data Command a b c = Command
    {
        input  :: a,
        output :: b,
        error  :: c
    }
    deriving(Show, Eq, Read, Data, Typeable, Generic)
    

data UserInput a = UserInput 
    {
        user_id :: EntityId,
        user_input :: a
    }
    deriving(Show, Eq, Read, Data, Typeable, Generic)
    
class UserCommand a where
    get_user_id :: a -> UserId
    
instance UserCommand (UserInput a) where
    get_user_id (UserInput x _) = x

data LoginInput   = LoginInput 
    {
        login_input_facebook_id :: FacebookId
    }
    deriving(Show, Eq, Read, Data, Typeable, Generic)
    
data LoginOutput  = LoginOutput FacebookSessionInfo
    deriving(Show, Eq, Read, Data, Typeable, Generic)
data LoginError   = LoginError FacebookSignupError
    deriving(Show, Eq, Read, Data, Typeable, Generic)
type LoginCommand = Command LoginInput LoginOutput LoginError


data ImageRating = ImageRating 
    {
        image_rating_image_id :: ImageId,
        image_rating_rating   :: Rating
    }
    deriving(Show, Eq, Read, Data, Typeable, Generic)
    

newtype RateInput = RateInput (UserInput ImageRating)  
    deriving(Show, Eq, UserCommand, Read, Data, Typeable, Generic)    

$(derive makeFrom ''RateInput)
    
data RateOutput = RateOutput
    deriving(Show, Eq, Read, Data, Typeable, Generic)
data RateError = RateError
    deriving(Show, Eq, Read, Data, Typeable, Generic)
type RateCommand = Command RateInput RateOutput RateError


data ImageData = ImageData 
    {
        image_data_bytes :: BS.ByteString,
        image_data_type :: String
    }
        deriving(Show, Eq, Read, Data, Typeable, Generic)

newtype UploadInput = UploadInput (UserInput ImageData)
    deriving(Show, Eq, UserCommand, Read, Data, Typeable, Generic)   

$(derive makeFrom ''UploadInput)

data UploadOutput = UploadOutput
    deriving(Show, Eq, Read, Data, Typeable, Generic)
data UploadError = UploadError
    deriving(Show, Eq, Read, Data, Typeable, Generic)
type UploadCommand = Command UploadInput UploadOutput UploadError


type Date = Int

data GetImagesType = New
                   | Hot
                   deriving(Eq, Show, Read, Ord, Data, Typeable, Generic)
                   
instance Hashable GetImagesType where
    hash New = 0
    hash Hot = 1
    
$(deriveAll ''GetImagesType)   

instance Normalize GetImagesType

instance G.GShow  GetImagesType where gshow      = G.gshowDefault
instance SEq      GetImagesType where shallowEq  = shallowEqDef
instance Build    GetImagesType where build      = buildDef
instance Children GetImagesType where children   = childrenDef
instance GDiff GetImagesType

data GetImagesInput = GetImagesInput
    {
        get_images_input_date :: Date,
        get_images_input_start :: Int,
        get_images_input_count :: Int,
        get_images_input_type :: GetImagesType
    }
        deriving(Show, Eq, Read, Data, Typeable, Generic)

type ImageEntityList = [ImageEntity]
        
data GetImagesOutput = GetImagesOutput
    {
        get_images_output_images :: ImageEntityList
    }
        deriving(Show, Eq, Read, Data, Typeable, Generic)
data GetImagesError = GetImageError
    deriving(Show, Eq, Read, Data, Typeable, Generic)
type GetImagesCommand = Command GetImagesInput GetImagesOutput GetImagesError

data PrintOption = State
                 | Leaderboards
    deriving(Show, Eq, Read, Data, Typeable, Generic)               

data Input    = InputLogin     LoginInput
              | InputRate      RateInput
              | InputUpload    UploadInput
              | InputGetImages GetImagesInput
              | Print          PrintOption
                  deriving(Show, Eq, Read, Data, Typeable, Generic)
              
data Output   = OutputLogin     LoginOutput
              | OutputRate      RateOutput
              | OutputUpload    UploadOutput
              | OutputGetImages GetImagesOutput
              | Empty
                  deriving(Show, Eq, Read, Data, Typeable, Generic)
                  

instance Normalize [ImageEntity] where
    normalize xs = sortBy (\x y -> entity_id y `compare` entity_id x) $ map normalize xs
instance Normalize Output
instance Normalize LoginOutput
instance Normalize RateOutput  
instance Normalize UploadOutput
instance Normalize GetImagesOutput            









            
            