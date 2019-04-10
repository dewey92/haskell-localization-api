{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Author.Apis.RegisterApi where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU
import Servant
import AppM
import Author.Model
import Author.Capability.Database

type RegisterApi = "register"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorInDb

-- | Payload
data AuthPayload = AuthPayload
  { email :: String
  , password :: String
  } deriving (Generic, FromJSON, ToJSON)

registerApi :: AuthPayload -> AppM AuthorInDb
registerApi authPayload =
  case mkPassword $ password (authPayload :: AuthPayload) of
    Nothing -> throwError $ err400 { errBody = "password not valid" }
    (Just password) -> do
      result <- register author' password
      case result of
        (Left e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
        (Right r) -> return r
  where
    author' = MkAuthor
      { authorEmail = Email $ email (authPayload :: AuthPayload)
      , authorName = Nothing
      }
