{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Author.Apis.Data where


import GHC.Generics
import Data.Aeson
import Data.Validation
import Author.Types

data AuthPayload = AuthPayload
  { email :: String
  , password :: String
  } deriving (Generic, FromJSON)

data ValidatedAuthPayload = ValidatedAuthPayload
  { vEmail :: Email
  , vPassword :: Password 'Raw
  }

validateAuthPayload :: AuthPayload -> Validation [AuthorValidationError] ValidatedAuthPayload
validateAuthPayload p
  = ValidatedAuthPayload
  <$> validateEmail (email p)
  <*> validatePassword (password p)
