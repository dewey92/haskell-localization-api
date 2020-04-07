{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ciliwung.AppM where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Ciliwung.Env (Env)
import Servant.Server (Handler, ServerError)

newtype AppM a = AppM (ReaderT Env Handler a) deriving
  ( Functor, Applicative, Monad
  , MonadReader Env, MonadIO
  , MonadError ServerError, MonadThrow, MonadCatch, MonadMask
  )

runAppM :: Env -> AppM a -> Handler a
runAppM env (AppM r) = runReaderT r env
