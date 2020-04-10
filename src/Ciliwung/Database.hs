{-# LANGUAGE DeriveAnyClass #-}

module Ciliwung.Database where

import Ciliwung.Author.Types (Email, Password, PasswordState(..))
import Control.Monad.Identity (Identity)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Text (Text)
import Database.Selda (SqlRow, Table, ID, Attr(..), table, autoPrimary)
import GHC.Generics (Generic)

data Author = Author
  { author_id :: ID Author
  , email :: Email
  , password :: Password 'Hashed
  , fullname :: Text
  } deriving (Generic, SqlRow)

instance ToJSON Author where
  toJSON entity = object
    [ "author_id" .= (show $ author_id entity)
    , "email" .= email entity
    , "fullname" .= fullname entity
    ]

authorTable :: Table Author
authorTable = table "author" [#author_id :- autoPrimary]
