{-# LANGUAGE NoMonomorphismRestriction #-}
module RouteRequests where
import Types
import Commands
import Control.Monad.Reader
import Control.Monad.Writer

persist_and_queue = undefined
handle_immediantly = undefined
    
route = do
    request_type <- asks incoming_payload_type
    case request_type of
        Put   -> persist_and_queue
        Get 0 -> handle_immediantly
        Get n -> persist_and_queue
