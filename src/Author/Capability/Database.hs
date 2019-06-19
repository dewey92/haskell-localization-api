{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Author.Capability.Database where

import GHC.Generics
import Data.Aeson
import Servant.Server (Handler)
import Author.Types

-- | Real data stored in database
data AuthorEntity = MkAuthorEntity
  { author_id :: Int
  , email :: Email
  , password :: Password 'Hashed
  , fullname :: String
  , created_at :: String
  , updated_at :: String
  }

-- | This capability represents the ability to manage authors or users in the system
class (Monad m) => MonadAuthorDb m where
  createAuthor :: Email -> Password 'Hashed -> m (Maybe AuthorEntity)
  findAuthorByEmail :: Email -> m (Maybe AuthorEntity)
