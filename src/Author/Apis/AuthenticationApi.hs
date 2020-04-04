{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Author.Apis.AuthenticationApi where

import AppM (AppM)
import Author.Types (AuthorEntity)
import Author.Apis.Data (AuthPayload(..))
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
registerApi (AuthPayload {..}) = do
  result <- registerAction email password
  case result of
    Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Right r -> return r

loginApi :: AuthPayload -> AppM AuthorEntity
loginApi (AuthPayload {..}) = do
  result <- loginAction email password
  case result of
    Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Right r -> return r
