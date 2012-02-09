{-# LANGUAGE NoMonomorphismRestriction #-}
module EntityGraphManipulation where
    
import Types
import Control.Monad.State
import Data.Graph.Inductive
import Evaluator

type GraphState = StateT EntityGraph
type Url = String


    
    