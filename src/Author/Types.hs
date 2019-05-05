{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Author.Types
  ( Author (..)
  , Email (..)
  , Password
  , showEmail
  , mkPassword
  , showPassword
  ) where

import GHC.Generics
import Data.Aeson

data Author = MkAuthor
  { authorEmail :: Email
  , authorName :: Maybe String
  } deriving (Eq, Generic)

instance ToJSON Author
instance FromJSON Author

-- | Email newtype
newtype Email = Email String deriving (Eq, ToJSON, FromJSON)

showEmail :: Email -> String
showEmail (Email e) = e

-- | Password newtype
newtype Password = Password String deriving (Eq)

mkPassword :: String -> Maybe Password
mkPassword p
  | length p < 8 = Nothing
  | otherwise = Just $ Password p

showPassword :: Password -> String
showPassword (Password p) = p
