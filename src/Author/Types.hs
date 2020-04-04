{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Author.Types
  ( Email
  , PasswordState(..)
  , Password
  , AuthorEntity(..)
  , AuthorErrors(..)
  , AuthorValidationError(..)
  , validateEmail
  , mkPassword
  , validatePassword
  , hashPassword
  ) where

import Control.Lens ((#))
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, object, (.=))
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Database.MySQL.Simple.Param (Param)
import Database.MySQL.Simple.Result (Result, convert)
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults, convertError)
import Data.Validation (_Success, _Failure, Validation(..))
import GHC.Generics (Generic)
import Text.Regex (mkRegex, matchRegex)

--------------------------------------------------------------------------------
-- Email
--------------------------------------------------------------------------------
newtype Email = Email { unEmail :: String  }
  deriving (Eq, Generic, ToJSON)
  deriving Param via String
  deriving Result via String

instance FromJSON Email where
  parseJSON raw = do
    str <- parseJSON raw
    case validateEmail str of
      Failure f -> fail $ show f
      Success email -> return email

validateEmail :: String -> Validation [AuthorValidationError] Email
validateEmail em
  | null em = _Failure # [EmailEmpty]
  | isNothing $ matchRegex regexEmail em = _Failure # [EmailNotValid]
  | otherwise = _Success # Email em
  where
    regexEmail = mkRegex "^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"

mkEmail :: String -> Maybe Email
mkEmail = toMaybe . validateEmail

--------------------------------------------------------------------------------
-- Password
--------------------------------------------------------------------------------
data PasswordState = Plain | Hashed

newtype Password (s :: PasswordState) = Password String deriving Eq
deriving instance Param (Password 'Hashed)
deriving instance Result (Password 'Hashed)

instance FromJSON (Password 'Plain) where
  parseJSON raw = do
    str <- parseJSON raw
    case validatePassword str of
      Failure f -> fail $ show f
      Success pw -> return pw

instance Show (Password ps) where
  show _ = "[FILTERED]"

validatePassword :: String -> Validation [AuthorValidationError] (Password 'Plain)
validatePassword pw
  | null pw = _Failure # [PasswordEmpty]
  | length pw < 8 = _Failure # [PasswordTooShort]
  | otherwise = _Success # Password pw

mkPassword :: String -> Maybe (Password 'Plain)
mkPassword = toMaybe . validatePassword

-- | TODO: implement hash
secureHash :: String -> String
secureHash = id

hashPassword :: Password 'Plain -> Password 'Hashed
hashPassword (Password r) = Password (secureHash r)

--------------------------------------------------------------------------------
-- Entity
--------------------------------------------------------------------------------
-- | Real data stored in database, might be moved to Capability.Database
data AuthorEntity = MkAuthorEntity
  { author_id :: Int
  , email :: Email
  , password :: Password 'Hashed
  , fullname :: String
  , created_at :: String
  , updated_at :: String
  }

instance QueryResults AuthorEntity where
  convertResults
    [f_authorId, f_email, f_pw, f_fullname, f_createdAt, f_updatedAt] -- `f` stands for `field`
    [v_id, v_email, v_pw, v_fullname, v_createdAt, v_updatedAt]
    = MkAuthorEntity author_id' email' password' fullname' created_at' updated_at'
    where
      !author_id' = convert f_authorId v_id
      !email' = convert f_email v_email
      !password' = convert f_pw v_pw -- Safe to coerce `Password` from DB
      !fullname' = convert f_fullname v_fullname
      !created_at' = convert f_createdAt v_createdAt
      !updated_at' = convert f_updatedAt v_updatedAt
  convertResults fs vs = convertError fs vs 6

instance ToJSON AuthorEntity where
  toJSON entity = object
    [ "author_id" .= author_id entity
    , "email" .= email entity
    , "fullname" .= fullname entity
    ]

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------
data AuthorErrors
  = EmailAlreadyExists
  | EmailNotExists
  | EmailAndPasswordNotMatch
  | OtherError String

instance Show AuthorErrors where
  show EmailAlreadyExists = "Email already exists"
  show EmailNotExists     = "Email is not found in the system"
  show EmailAndPasswordNotMatch = "Email and password don't match"
  show (OtherError msg)   = msg

data AuthorValidationError
  = PasswordTooShort
  | PasswordEmpty
  | EmailEmpty
  | EmailNotValid

instance Show AuthorValidationError where
  show PasswordEmpty = "Psasword should not be empty"
  show EmailEmpty    = "Email should not be empty"
  show EmailNotValid = "Not a valid email"

-- | Utils
toMaybe :: Validation e a -> Maybe a
toMaybe v = case v of
  (Failure _) -> Nothing
  (Success a) -> Just a
