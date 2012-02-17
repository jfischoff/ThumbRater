{-# LANGUAGE NoMonomorphismRestriction #-}
module Parser where

import Commands
import Types    
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import qualified Data.ByteString.Char8 as BSC
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)

parse_commands = (do {optional spaces; parse_command}) `sepBy` newline

parse_command =  try parse_login
             <|> try parse_rate
             <|> try parse_upload
             <|> try parse_get_images
             <|> try parse_print
             
parse_print = do
             string "print" 
             spaces
             Print <$> parse_print_option
             
parse_print_option = try (parse_name "state" Commands.State)
                  <|> try (parse_name "leaderboards" Leaderboards)
                   
parse_name name output = do
    string name
    return output
             
parse_login = do 
    string "login"
    spaces
    fb_id <- parse_fb_id
    return $ InputLogin $ LoginInput fb_id
    
parse_rate = do
    string "rate"
    uc <- parse_user_command parse_image_rating
    return $ InputRate $ RateInput uc
    
parse_image_rating = do
    spaces
    image_id <- (integer haskell)
    spaces 
    rating <- parse_rating
    return $ ImageRating (fromIntegral image_id) rating
    
parse_upload = do
    string "upload"
    uc <- parse_user_command parse_image_data
    return $ InputUpload $ UploadInput $ uc
    
parse_get_images = do
    string "get_images"
    spaces 
    date  <- (integer haskell)
    start <- (integer haskell)
    count <- (integer haskell)
    typ   <- parse_leaderboard_type
    
    return $ InputGetImages $ GetImagesInput (fromIntegral date) (fromIntegral start) 
        (fromIntegral count) typ
    
parse_leaderboard_type =  try parse_hot
                      <|> try parse_new
                      
parse_hot = do
    choice [string "Hot", string "hot"]
    return Hot   
    
parse_new = do
    choice [string "New", string "new"]
    return New
    
parse_image_data = do
    spaces
    image_bytes <- parse_image_bytes
    spaces
    image_type <- parse_image_type
    return $ ImageData image_bytes image_type
    
parse_image_bytes = do
    chars <- manyTill anyChar (try space)
    return $ BSC.pack chars
    
parse_image_type = (identifier haskell)
    
parse_user_command p = do
    spaces
    user_id <- parse_user_id
    spaces
    value   <- p
    return $ UserInput (fromIntegral user_id) value
    
parse_rating =  try parse_thumbs_up
            <|>     parse_thumbs_down
              
parse_thumbs_up = do
    choice [string "ThumbsUp", string "thumbs_up", string "thumbs up"]
    return ThumbsUp
    
parse_thumbs_down = do
    choice [string "ThumbsDown", string "thumbs_down", string "thumbs down"]
    return ThumbsDown
    
parse_user_id = do
    user_id <- (integer haskell)
    return user_id
    
parse_fb_id = do
    fb_id <- manyTill anyChar (try space)
    return fb_id
