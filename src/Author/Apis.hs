module Author.Apis
  ( module Author.Apis.RegisterApi
  , AuthorApis
  , authorApis
  ) where

import Author.Apis.RegisterApi

type AuthorApis = RegisterApi

authorApis = registerApi
