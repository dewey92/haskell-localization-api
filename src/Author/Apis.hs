{-# LANGUAGE TypeOperators #-}

module Author.Apis
  ( module Author.Apis.AuthenticationApi
  , AuthorApis
  , authorApis
  ) where

import Servant
import Author.Apis.AuthenticationApi

type AuthorApis = RegisterApi :<|> LoginApi

authorApis = registerApi :<|> loginApi
