{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AppM where

import Control.Monad.Reader
import Servant.Server
import Author.Capability.Database
import qualified Author.Capability.MySQLDatabase as AuthorMySQL
import Types

-- | TODO: Change Env
type AppM = ReaderT Env Handler

-- | Grant `AppM` the capabilities to access Author Database. I'm using -XTypeApplications
-- | so that the implementation can be extracted to another file instead of
-- | polluting in this file. It would be too long and will become unreadable if put here
-- | since the purpose of this module is just to define all capabilities of `AppM`
instance MonadAuthorDb AppM where
  createAuthor = AuthorMySQL.createAuthor
  findAuthorByEmail = AuthorMySQL.findAuthorByEmail
