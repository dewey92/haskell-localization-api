{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Author.Apis.AuthenticationApi where

import GHC.Generics
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU
import Servant
import AppM
import Author.Types
import Author.Model
import Author.Capability.Database (AuthorEntity (..))

-- | All authentication API types defined here
type RegisterApi = "register"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity
type LoginApi = "login"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity

-- | Payload type, used by Register and Login endpoints
data AuthPayload = AuthPayload
  { email :: String
  , password :: String
  } deriving (Generic, FromJSON)

-- | Hide unneccessary fields on `AuthorEntity`
-- instance Generic AuthorEntity
instance ToJSON AuthorEntity where
  toJSON entity = object
    [ "author_id" .= author_id entity
    , "email" .= email (entity :: AuthorEntity)
    , "fullname" .= fullname entity
    ]

registerApi :: AuthPayload -> AppM AuthorEntity
registerApi authPayload =
  case mkPassword $ password (authPayload :: AuthPayload) of
    Nothing -> throwError $ err400 { errBody = "password not valid" }
    (Just password) -> do
      result <- registerAction validatedEmail password
      case result of
        (Left e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
        (Right r) -> return r
  where
    validatedEmail = Email $ email (authPayload :: AuthPayload)

loginApi :: AuthPayload -> AppM AuthorEntity
loginApi authPayload =
  case mkPassword $ password (authPayload :: AuthPayload) of
    Nothing -> throwError $ err400 { errBody = "password not valid" }
    (Just password) -> do
      result <- loginAction validatedEmail password
      case result of
        (Left e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
        (Right r) -> return r
  where
    validatedEmail = Email $ email (authPayload :: AuthPayload)
