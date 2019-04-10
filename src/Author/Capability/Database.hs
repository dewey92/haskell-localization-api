{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Author.Capability.Database where

import GHC.Generics
import Data.Aeson
import Servant.Server (Handler)
import Author.Model

-- | Records in the database
-- | TODO: Remove created_at & updated_at?
data AuthorInDb = AuthorInDb
  { author_id :: Int
  , email :: Email
  , password :: String
  , fullname :: String
  , created_at :: String
  , updated_at :: String
  } deriving (Generic, FromJSON)

-- | FIXME: Not sure if I put it right here. It shouldn't be the focus
-- | of a database module to define what data to be hidden and what to be
-- | shown to end-users. On top of that, this module SHOULD be ignorant
-- | of what kind of data to be presented, be it as JSON, XML, or plain text.
instance ToJSON AuthorInDb where
  toJSON AuthorInDb {..} = object
    [ "author_id" .= author_id
    , "email" .= email
    , "fullanme" .= fullname
    ]

-- | All kinds of database errors dealing with Author's domain
data AuthorDbErrors
  = EmailAlreadyExists
  | EmailAndPasswordNotMatch
  | EmailNotExists

instance Show AuthorDbErrors where
  show EmailAlreadyExists = "Email already exists"
  show EmailAndPasswordNotMatch = "Email and password don't match"
  show EmailNotExists = "Email is not found in the system"

-- | This capability represents the ability to manage authors or users in the system
class Monad m => AuthorMonadDb m where
  register :: Author -> Password -> m (Either AuthorDbErrors AuthorInDb)
  login :: Email -> Password -> m (Either AuthorDbErrors AuthorInDb)
  findAuthorByEmail :: Email -> m (Maybe AuthorInDb)
