module EvaluatorTest where
import qualified Data.HashMap.Strict as H
import Evaluator 
import Parser
import Data.IORef
import Data.Graph.Inductive
import PushApple.AppContext
import Control.Monad
import Control.Concurrent
import Text.Parsec
import Commands
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Either
import System.Exit

isRight (Right x) = True
isRight _ = False

fromRight (Right x) = x


run_parser input = runParser parse_command () "" input

--run_context' = undefined

--run_context' default_state default_config commands = 
--    (runStateT (runReaderT (runWriterT commands) default_config) 
--            default_state)

--run_evaluator :: AppReadonly -> AppState -> Input -> IO ((Output, AppState), ())
run_evaluator x y input = run_context y x (runApp $ eval input)

read_only = AppReadonly [] H.empty

main = do
    
    let app_state = AppState empty
    app_state_ref <- newIORef app_state
    
    loop app_state_ref
    
abort_if line str = do
    if line == str
        then exitSuccess
        else return ()
        
--todo
--make the other evaluator
--make the serialization

loop app_state_ref = do
    state <- readIORef app_state_ref
    
    line <- getLine
    
    abort_if line "q"
    
    let parse_output = run_parser line
    print parse_output
    if isRight parse_output
        then do
                let input = fromRight parse_output
    
                ((output, ()), new_state) <- run_evaluator read_only state input
    
                print output
                print new_state
    
                writeIORef app_state_ref new_state
                
                loop app_state_ref
                
        else loop app_state_ref
                    
 