{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, StandaloneDeriving,
    DeriveGeneric, DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances,
    FlexibleInstances, DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, 
    MultiParamTypeClasses, UndecidableInstances #-}
module ErrorTest where
    
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Error as Error 
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.RWS.Strict as RWS

data ErrorType = Err String
    deriving(Show)

instance Error.Error ErrorType where
    noMsg    = Err "hey"
    strMsg x = Err x

type SuperStateType reader writer state err core output = 
    Error.ErrorT err (RWS.RWST reader writer state core) output

newtype SuperStateT reader writer state err core output = SuperStateT {
    un_state :: SuperStateType reader writer state err core output}
    deriving(Monad, Trans.MonadIO, RWS.MonadRWS reader writer state, 
             Reader.MonadReader reader, Writer.MonadWriter writer, State.MonadState state, Functor)
             
runSuperState action = RWS.runRWST (Error.runErrorT (un_state action))
                    
instance (Monad core, Error.Error err, RWS.Monoid writer) =>  
          Error.MonadError err (SuperStateT reader writer state err core) where
    throwError = SuperStateT . Error.ErrorT . return . Left 
    action `catchError` handler = SuperStateT $ un_state action `Error.catchError` \e -> un_state (handler e)
    
{-

type TestState = SuperStateT String String Int ErrorType IO


main = runSuperState (actions `Error.catchError` (\e -> do {RWS.liftIO $ print e;})) "" 1 

actions = do
    x <- action_0
    RWS.liftIO $ print x 
    
    y <- action_1
    RWS.liftIO $ print y
    
    z <- action_2
    RWS.liftIO $ print z

action_0 = do 
    RWS.modify (1+)
    RWS.get
    
action_1 :: TestState Int
action_1 = do
    Error.throwError $ Error.strMsg "shit!"
    RWS.get
    
action_2 = do
    RWS.modify (1+)
    RWS.get

-}






