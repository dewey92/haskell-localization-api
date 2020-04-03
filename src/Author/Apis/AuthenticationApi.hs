{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Author.Apis.AuthenticationApi where

import AppM (AppM)
import Author.Types (AuthorEntity)
import Author.Apis.Data (AuthPayload(..), ValidatedAuthPayload(..), validateAuthPayload)
import Author.Model
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Validation
import GHC.Generics (Generic)
import Servant

-- | All authentication API types defined here
type RegisterApi = "register"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity
type LoginApi = "login"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] AuthorEntity

registerApi :: AuthPayload -> AppM AuthorEntity
registerApi authPayload =
  case validateAuthPayload authPayload of
    Failure e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Success (ValidatedAuthPayload { vEmail, vPassword }) -> do
      result <- registerAction vEmail vPassword
      case result of
        Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
        Right r -> return r

loginApi :: AuthPayload -> AppM AuthorEntity
loginApi authPayload =
  case validateAuthPayload authPayload of
    Failure e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Success (ValidatedAuthPayload { vEmail, vPassword }) -> do
      result <- loginAction vEmail vPassword
      case result of
        Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
        Right r -> return r
