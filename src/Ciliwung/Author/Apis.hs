{-# LANGUAGE TypeOperators #-}

module Ciliwung.Author.Apis
  ( module Ciliwung.Author.Apis.AuthenticationApi
  , AuthorApis
  , authorApis
  ) where

import Servant
import Ciliwung.Author.Apis.AuthenticationApi

type AuthorApis = RegisterApi :<|> LoginApi

authorApis = registerApi :<|> loginApi
