{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ciliwung.Author.Types
  ( Email
  , PasswordState(..)
  , Password
  , AuthorErrors(..)
  , AuthorValidationError(..)
  , validateEmail
  , validatePassword
  , hashPassword
  ) where

import Control.Lens ((#))
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, object, (.=))
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Validation (_Success, _Failure, Validation(..))
import Data.Text (Text, pack, unpack)
import Database.Selda.SqlType (SqlType, SqlTypeRep(..), SqlValue(..), Lit(..), mkLit, defaultValue, fromSql)
import GHC.Generics (Generic)
import Text.Regex (mkRegex, matchRegex)

--------------------------------------------------------------------------------
-- Email
--------------------------------------------------------------------------------
newtype Email = Email Text
  deriving (Eq, Show, Read, Generic, ToJSON)

instance SqlType Email where
  mkLit (Email e) = LCustom TText $ LText e
  defaultValue = LCustom TText $ LText mempty
  fromSql = \case
    SqlString s -> Email s
    _           -> error $ "fromSql: bad email string"

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
  | otherwise = _Success # Email (pack em)
  where
    regexEmail = mkRegex "^[_a-zA-Z0-9-]+(\\.[_a-zA-Z0-9-]+)*@[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*\\.(([0-9]{1,3})|([a-zA-Z]{2,3})|(aero|coop|info|museum|name))$"

--------------------------------------------------------------------------------
-- Password
--------------------------------------------------------------------------------
data PasswordState = Plain | Hashed

newtype Password (s :: PasswordState) = Password Text
  deriving Eq

instance SqlType (Password 'Hashed) where
  mkLit (Password p) = LCustom TText $ LText p
  defaultValue = LCustom TText $ LText mempty
  fromSql = \case
    SqlString p -> Password p
    _           -> error $ "fromSql: bad password string"

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
  | otherwise = _Success # Password (pack pw)

-- | TODO: implement hash
secureHash :: Text -> Text
secureHash = id

hashPassword :: Password 'Plain -> Password 'Hashed
hashPassword (Password r) = Password (secureHash r)

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
  = PasswordEmpty
  | PasswordTooShort
  | EmailEmpty
  | EmailNotValid

instance Show AuthorValidationError where
  show PasswordEmpty = "Psasword should not be empty"
  show PasswordTooShort = "Password show be longer than 8 chars"
  show EmailEmpty    = "Email should not be empty"
  show EmailNotValid = "Not a valid email"

-- | Utils
toMaybe :: Validation e a -> Maybe a
toMaybe v = case v of
  (Failure _) -> Nothing
  (Success a) -> Just a
