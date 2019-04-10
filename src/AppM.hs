{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppM where

import Servant.Server
import Control.Monad.Reader
import Control.Monad.Except
import Data.Cache
import Author.Capability.Database
import Author.Instance.InMemoryDatabase

-- | TODO: Change Env
type Env = String

newtype AppM a = AppM { unAppM :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr)

-- | Grant `AppM` the capabilities to access Author Database. I'm using -XTypeApplications
-- | so that the implementation can be extracted to another file instead of
-- | polluting in this file. It would be just too long and will become unreadable if put here
-- | since the purpose of this module is just to define all capabilities of `AppM`
instance AuthorMonadDb AppM where
  register a = liftIO . unInMemory . register @InMemory a
  login a = liftIO . unInMemory . login @InMemory a
  findAuthorByEmail = liftIO . unInMemory . findAuthorByEmail @InMemory
