{-# LANGUAGE OverloadedStrings #-}

module Author.Capability.Database where

import GHC.Generics
import Data.Aeson
import Servant.Server (Handler)
import Author.Types

-- | Real data stored in database
data AuthorEntity = MkAuthorEntity
  { author_id :: Int
  , email :: Email
  , password :: Password
  , fullname :: String
  , created_at :: String
  , updated_at :: String
  }

-- | All kinds of database errors dealing with Author's domain
data AuthorDbErrors
  = EmailAlreadyExists
  | EmailAndPasswordNotMatch
  | EmailNotExists
  | OperationFailed

instance Show AuthorDbErrors where
  show EmailAlreadyExists = "Email already exists"
  show EmailAndPasswordNotMatch = "Email and password don't match"
  show EmailNotExists = "Email is not found in the system"
  show OperationFailed = "Operation failed"

-- | This capability represents the ability to manage authors or users in the system
class (Monad m) => MonadAuthorDb m where
  createAuthor :: Email -> Password -> m (Either AuthorDbErrors AuthorEntity)
  findAuthorByEmail :: Email -> m (Maybe AuthorEntity)
