{-# LANGUAGE RecordWildCards #-}

module Ciliwung.Author.Apis.AuthenticationApi where

import Ciliwung.AppM (AppM)
import Ciliwung.Database (Author)
import Ciliwung.Author.Apis.Data (AuthPayload(..))
import Ciliwung.Author.Model
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Validation
import GHC.Generics (Generic)
import Servant

-- | All authentication API types defined here
type RegisterApi = "register"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] Author
type LoginApi = "login"
  :> ReqBody '[JSON] AuthPayload
  :> Post '[JSON] Author

registerApi :: AuthPayload -> AppM Author
registerApi (AuthPayload {..}) = do
  result <- registerAction email password
  case result of
    Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Right r -> return r

loginApi :: AuthPayload -> AppM Author
loginApi (AuthPayload {..}) = do
  result <- loginAction email password
  case result of
    Left e -> throwError $ err400 { errBody = BLU.fromString $ show e }
    Right r -> return r
