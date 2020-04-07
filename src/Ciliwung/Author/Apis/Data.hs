{-# LANGUAGE DeriveAnyClass #-}

module Ciliwung.Author.Apis.Data where

import Ciliwung.Author.Types
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
