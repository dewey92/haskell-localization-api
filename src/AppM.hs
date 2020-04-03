{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Env (Env)
import Servant.Server (Handler, ServantErr)

newtype AppM a = AppM (ReaderT Env Handler a) deriving
  (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServantErr)

runAppM :: Env -> AppM a -> Handler a
runAppM env (AppM r) = runReaderT r env
