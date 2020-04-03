{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Author.Apis.Data where


import Author.Types
  ( Email
  , Password
  , PasswordState(..)
  , AuthorValidationError
  , validateEmail
  , validatePassword
  )
import Data.Aeson (FromJSON)
import Data.Validation (Validation)
import GHC.Generics (Generic)

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
