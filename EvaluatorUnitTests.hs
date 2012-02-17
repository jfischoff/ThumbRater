{-# LANGUAGE TemplateHaskell, FlexibleContexts, 
    QuasiQuotes, NoMonomorphismRestriction #-}
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

module EvaluatorUnitTests where
import Evaluator

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit
import Commands hiding (error)
import Types
import Control.Arrow hiding ((<+>))
import GHC.Generics
import Control.Monad
import Data.Graph.Inductive
import Data.List
import qualified Data.HashMap.Strict as H
import StateParser
import Data.String.Interpolation
import Text.Parsec hiding (parse, string)
import CommandQuasi
import Control.Applicative
-- import Generics.Instant.GDiff
import Data.Lens.Strict
import Normalize 
import NormalizeInstances
import Text.Groom
import Data.Maybe
import Data.Generic.Diff
import StateDiff
import Commands
--import ColorDiff
import System.Console.ANSI ( Color(Green, Red, Black), ColorIntensity(Dull)
                           , ConsoleLayer(Foreground), SGR(SetColor, Reset)
                           , setSGRCode )
import Text.PrettyPrint.Free
import Parser
                           

main = defaultMain tests

tests = [
        testGroup "Rate Command" [
                testCase "add_leaderboard_to_hashmap" test_add_leaderboard_to_hashmap,
                testCase "update_or_add_leaderboard_empty'" test_update_or_add_leaderboard_empty,
                testCase "test_update_or_add_leaderboard_append'" test_update_or_add_leaderboard_append,
                testCase "add_rating_0" add_rating_test_0,
                testCase "add_rating_1" add_rating_test_1
        ],
        testGroup "Login Command" [
            testCase "test_login_create" test_login_create
        ], 
        testGroup "Upload Command" [
            testCase "test_upload" test_upload
        ], 
        testGroup "GetImages Command" [
            testCase "test_get_images" test_get_images
        ]
        
    ]

--instance Normalize AppStateTuple where






test_add_leaderboard_to_hashmap = actual @?= expected where
    actual = map (second H.toList) $ H.toList $ add_leaderboard_to_hashmap Hot (1::Int) ["hey"] (H.fromList [])
    expected = [(Hot, [(1, ["hey"])])]


test_update_or_add_leaderboard_empty = actual @?= expected where
    actual   = map (second H.toList) $ H.toList $ update_or_add_leaderboard New (1::Int) ["hey"] (H.fromList [])
    expected = [(New, [(1, ["hey"])])]
    
test_update_or_add_leaderboard_append = actual @?= expected where
    actual   = map (second H.toList) $ H.toList $ update_or_add_leaderboard New (1::Int) ["new"] $ H.singleton New $ H.singleton 1 ["hey"]
    expected = [(New, [(1, ["new", "hey"])])]
    
--I want some way to describe the state
run_evaluator' input x = run_evaluator (AppReadonly "" False) x input

run_cmds cmds x = foldM (\l c -> run_and_accum c l) ([], x) cmds where
    run_and_accum c (accum, state) = do 
        (new, new_state, ()) <- run_evaluator' c state
        return (new:accum, new_state)
    
fromRight (Right x) = x
fromRight (Left x) =  Prelude.error (show x)

mk_diffable :: AppState -> AppStateTuple
mk_diffable (AppState g x y t) = normalize (decompose_graph g, map (second H.toList) $ H.toList x, H.toList y, t)

-- (@?==) :: (Show a, Eq a, Normalize a, Family f, Type f a) => a -> a -> Assertion
(@?==) :: AppStateTuple -> AppStateTuple -> Assertion
x @?== y =
    assertBool ('\n':msg) (x == y)
  where
    msg = (displayS $ renderCompact
     $ fmt $ compress $ diff_app x y) ""
    
diff_app :: AppStateTuple -> AppStateTuple -> EditScript AppStateTupleFam AppStateTuple AppStateTuple
diff_app x y = diff x y


state_0 = fromRight $ parse [str|
                        time 1329350400
                        images 3 1.png                      
                               4 2.png                      
                                    
                        users 1 08653456                    
                                images 3                    
                                    
                                ratings 5 3 ThumbsDown      
                                    
                              2 00763623                    
                                images 4                    
                                    
                                ratings 6 4 ThumbsUp      
                                
                        fb_map 08653456 1                   
                               00763623 2  
                        
                        leaderboards
                            New  1329350400 3                         
                                      4      
       
                            Hot  1329350400 4
                                      3  
                 
                                                |]
                                                
state_after_rating = fromRight $ parse [str|
                        time 1329350400
                        images 3 1.png                      
                               4 2.png                      

                        users 1 08653456                    
                                images 3                    

                                ratings 5 3 ThumbsDown
                                        7 4 ThumbsDown      

                              2 00763623                    
                                images 4                    

                                ratings 6 4 ThumbsUp      

                        fb_map 08653456 1                   
                               00763623 2  

                        leaderboards
                            New  1329350400 3                         
                                      4      

                            Hot  1329350400 4
                                      3  

                                                |]
                                                
                                                
add_rating_test_0 = do 
    actual <- (\(Right _, x, _) -> x) <$> run_evaluator' [cmd|rate 1 4 ThumbsDown|] state_0    
    mk_diffable actual @?== mk_diffable state_after_rating
    
state_after_rating_1 = fromRight $ parse [str|
        time 1329350400
        images 3 1.png                      
               4 2.png                      

        users 1 08653456                    
                images 3                    

                ratings 5 3 ThumbsDown
                        7 3 ThumbsUp
                        8 3 ThumbsUp      

              2 00763623                    
                images 4                    

                ratings 6 4 ThumbsUp      

        fb_map 08653456 1                   
               00763623 2  

        leaderboards
            New  1329350400 3                         
                      4      

            Hot  1329350400 4
                      3  

                      |]



add_rating_test_1 = do                                                
    actual <- snd <$> run_cmds [cmds|
        rate 1 3 ThumbsUp
        rate 1 3 ThumbsUp |] state_0    
    mk_diffable actual @?== mk_diffable state_after_rating_1
    

login_create_expected = fromRight $ parse [str|
                        time 1329350400
                        images 3 1.png                      
                               4 2.png                      

                        users 1 08653456                    
                                images 3                    

                                ratings 5 3 ThumbsDown      

                              2 00763623                    
                                images 4                    

                                ratings 6 4 ThumbsUp   
                                
                              7 00112233  

                        fb_map 08653456 1                   
                               00763623 2  
                               00112233 7

                        leaderboards
                            New  1329350400 3                         
                                      4      

                            Hot  1329350400 4
                                      3  

                                                |]
       
run_0 cmds expected = do 
    actual <- snd <$> run_cmds cmds state_0
    mk_diffable expected @?== mk_diffable actual

test_login_create = run_0 [cmds| login 00112233 |] login_create_expected

upload_expected = fromRight $ parse [str|
                        time 1329350400
                        images 3 1.png                      
                               4 2.png        
                               7 7.png              

                        users 1 08653456                    
                                images 3     
                                       7               

                                ratings 5 3 ThumbsDown      

                              2 00763623                    
                                images 4                    

                                ratings 6 4 ThumbsUp   
                                

                        fb_map 08653456 1                   
                               00763623 2  
                               
                        leaderboards
                            New  1329350400 7
                                      3                         
                                      4      

                            Hot  1329350400 4
                                      7
                                      3 

                                                |]

run_parser input = fromRight $ runParser parse_command () "" input

test_upload = run_0 ([run_parser "upload 1 453152 png"]) upload_expected

instance (Normalize b) => Normalize (Either a b) where
    normalize (Right x) = Right (normalize x)
    normalize x = x


    

test_get_0 cmd expected = do
    (actual, new_state, _) <- run_evaluator' cmd state_0
    mk_diffable new_state @?== mk_diffable state_0
    normalize (Right expected)  @?= normalize actual

test_get_images = test_get_0 [cmd|get_images 1329350400 0 15 Hot |] $ 
                    OutputGetImages $ GetImagesOutput [
                        Entity 4 $ ImageInfo "png"
                        "4.png"
                        [ThumbsUp]
                        1,
                        Entity 3 $ ImageInfo "png"
                        "3.png"
                        [ThumbsDown]
                        2
                    ]
                            

































                                    
state_1 = parse [str|
                        images 3 1.png                      
                               4 2.png                      
                                    
                        users 1 08653456                    
                                images 3                    
                                    
                                ratings 5 3 ThumbsDown      
                                    
                              2 00763623                    
                                images 4                    
                                    
                                ratings 5 4 ThumbsUp        
                                    
                        fb_map 08653456 1                   
                               00763623 2                   |]
       
       
images_0 = runParser parse_images () "" " images 3 1.png  \n4 2.png  \n"
users_0  = runParser parse_users () "" [str| 
                                                users 1 08653456                    
                                                        images 3                    
                                    
                                                        ratings 5 3 ThumbsDown       
                                     
                                                        2 00763623                   
                                                          images 4                   
                                     
                                                          ratings 5 4 ThumbsUp       |]
          
leaderboards_0 = runParser parse_leaderboards () "" [str|
                                                            New  1000 3                         
                                                                      4                         
                                    
                                                            Hot  1000 4                         
                                                                      3                         
                                                                      |]
       
fb_map_0 = runParser parse_fb_map () "" [str|
                                                fb_map 08653456 1                   
                                                       00763623 2 |]


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
