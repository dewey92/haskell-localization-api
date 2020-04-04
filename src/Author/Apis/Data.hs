{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Author.Apis.Data where

import Author.Types
  ( Email
  , Password
  , PasswordState(..)
  )
import Data.Aeson (FromJSON)
import Data.Validation (Validation)
import GHC.Generics (Generic)

data AuthPayload = AuthPayload
  { email :: Email
  , password :: Password 'Plain
  } deriving (Generic, FromJSON)
