{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module Commands where
import Data.DeriveTH
import Types
import qualified Data.ByteString as BS
import Data.Hashable
import Data.Aeson

type FacebookSessionInfo = String
type FacebookSignupError = String


data Command a b c = Command
    {
        input  :: a,
        output :: b,
        error  :: c
    }
    deriving(Show, Eq, Read)
    

data UserInput a = UserInput 
    {
        user_id :: EntityId,
        user_input :: a
    }
    deriving(Show, Eq, Read)
    
class UserCommand a where
    get_user_id :: a -> UserId
    
instance UserCommand (UserInput a) where
    get_user_id (UserInput x _) = x

data LoginInput   = LoginInput 
    {
        login_input_facebook_id :: FacebookId
    }
    deriving(Show, Eq, Read)
    
data LoginOutput  = LoginOutput FacebookSessionInfo
    deriving(Show, Eq, Read)
data LoginError   = LoginError FacebookSignupError
    deriving(Show, Eq, Read)
type LoginCommand = Command LoginInput LoginOutput LoginError


data ImageRating = ImageRating 
    {
        image_rating_image_id :: ImageId,
        image_rating_rating   :: Rating
    }
    deriving(Show, Eq, Read)
    

newtype RateInput = RateInput (UserInput ImageRating)  
    deriving(Show, Eq, UserCommand, Read)    

$(derive makeFrom ''RateInput)
    
data RateOutput = RateOutput
    deriving(Show, Eq, Read)
data RateError = RateError
    deriving(Show, Eq, Read)
type RateCommand = Command RateInput RateOutput RateError


data ImageData = ImageData 
    {
        image_data_bytes :: BS.ByteString,
        image_data_type :: String
    }
        deriving(Show, Eq, Read)

newtype UploadInput = UploadInput (UserInput ImageData)
    deriving(Show, Eq, UserCommand, Read)   

$(derive makeFrom ''UploadInput)

data UploadOutput = UploadOutput
    deriving(Show, Eq, Read)
data UploadError = UploadError
    deriving(Show, Eq, Read)
type UploadCommand = Command UploadInput UploadOutput UploadError


type Date = Int

data GetImagesType = New
                   | Hot
                   deriving(Eq, Show, Read)
                   
instance Hashable GetImagesType where
    hash New = 0
    hash Hot = 1

data GetImagesInput = GetImagesInput
    {
        get_images_input_date :: Date,
        get_images_input_start :: Int,
        get_images_input_count :: Int,
        get_images_input_type :: GetImagesType
    }
        deriving(Show, Eq, Read)
        
data GetImagesOutput = GetImagesOutput
    {
        get_images_output_images :: [ImageInfo]
    }
        deriving(Show, Eq, Read)
data GetImagesError = GetImageError
    deriving(Show, Eq, Read)
type GetImagesCommand = Command GetImagesInput GetImagesOutput GetImagesError


data Input    = InputLogin     LoginInput
              | InputRate      RateInput
              | InputUpload    UploadInput
              | InputGetImages GetImagesInput
                  deriving(Show, Eq, Read)
              
data Output   = OutputLogin     LoginOutput
              | OutputRate      RateOutput
              | OutputUpload    UploadOutput
              | OutputGetImages GetImagesOutput
                  deriving(Show, Eq, Read)
              









            
            