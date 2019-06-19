{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Author.Types
  ( Email
  , PasswordState (..)
  , Password
  , AuthorValidationError (..)
  , validateEmail
  , mkPassword
  , validatePassword
  , hashPassword
  ) where

import GHC.Generics
import Control.Lens
import Data.Validation
import Data.Maybe (isNothing)
import Text.Regex (mkRegex, matchRegex)
-- Capabilities. Not sure if this is the right pattern
import Data.Aeson hiding (Success, Result)
import Database.MySQL.Simple.Param (Param)
import Database.MySQL.Simple.Result (Result)

newtype Email = Email { unEmail :: String  } deriving (Eq, Generic)

data PasswordState = Raw | Hashed

newtype Password (s :: PasswordState) = Password String deriving (Eq)

data AuthorValidationError
  = PasswordTooShort
  | PasswordEmpty
  | EmailEmpty
  | EmailNotValid

instance Show AuthorValidationError where
  show PasswordEmpty = "Psasword should not be empty"
  show PasswordTooShort = "Password should contain at least 8 characters"
  show EmailEmpty = "Email should not be empty"
  show EmailNotValid = "Not a valid email"

validateEmail :: String -> Validation [AuthorValidationError] Email
validateEmail em
  | null em = _Failure # [EmailEmpty]
  | isNothing $ matchRegex regexEmail em = _Failure # [EmailNotValid]
  | otherwise = _Success # Email em
  where
    regexEmail = mkRegex "^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"

mkEmail :: String -> Maybe Email
mkEmail = toMaybe . validateEmail

validatePassword :: String -> Validation [AuthorValidationError] (Password 'Raw)
validatePassword pw
  | null pw = _Failure # [PasswordEmpty]
  | length pw < 8 = _Failure # [PasswordTooShort]
  | otherwise = _Success # Password pw

mkPassword :: String -> Maybe (Password 'Raw)
mkPassword = toMaybe . validatePassword

-- | TODO: implement hash
secureHash :: String -> String
secureHash = id

hashPassword :: Password 'Raw -> Password 'Hashed
hashPassword (Password r) = Password (secureHash r)

-- | Utils
toMaybe :: Validation e a -> Maybe a
toMaybe v = case v of
  (Failure _) -> Nothing
  (Success a) -> Just a

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------
instance FromJSON Email
instance ToJSON Email
deriving instance Param Email
deriving instance Result Email

deriving instance Param (Password 'Hashed)
deriving instance Result (Password 'Hashed)
