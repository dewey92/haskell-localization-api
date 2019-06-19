{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Author.Apis.AuthenticationApi where

import GHC.Generics
import Control.Monad.Reader
import Data.Aeson hiding (Success)
import Data.Validation
import Data.ByteString.Lazy.UTF8 as BLU
import Servant
import AppM
import Author.Types
import Author.Model
import Author.Apis.Data
import Author.Capability.Database (AuthorEntity (..))

-- | All authentication API types defined here
type RegisterApi = "register"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity
type LoginApi = "login"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity

-- | Hide unneccessary fields on `AuthorEntity`
instance ToJSON AuthorEntity where
  toJSON entity = object
    [ "author_id" .= author_id entity
    , "email" .= email (entity :: AuthorEntity)
    , "fullname" .= fullname entity
    ]

registerApi :: AuthPayload -> AppM AuthorEntity
registerApi authPayload =
  case validateAuthPayload authPayload of
    (Failure e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
    (Success vAuthPayload) -> do
      result <- registerAction (vEmail vAuthPayload) (vPassword vAuthPayload)
      case result of
        (Left e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
        (Right r) -> return r

loginApi :: AuthPayload -> AppM AuthorEntity
loginApi authPayload =
  case validateAuthPayload authPayload of
    (Failure e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
    (Success vAuthPayload) -> do
      result <- loginAction (vEmail vAuthPayload) (vPassword vAuthPayload)
      case result of
        (Left e) -> throwError $ err400 { errBody = BLU.fromString $ show e }
        (Right r) -> return r
