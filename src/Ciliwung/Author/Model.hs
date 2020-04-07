module Ciliwung.Author.Model where

import Ciliwung.Author.Types
  ( AuthorErrors(..)
  , Email
  , Password
  , PasswordState(..)
  , hashPassword
  )
import Ciliwung.Author.Capability.ManageAuthor (ManageAuthor, createAuthor, findAuthorByEmail)
import Ciliwung.Database (Author(..))
import Control.Arrow (left)

registerAction
  :: ManageAuthor m
  => Email
  -> Password 'Plain
  -> m (Either AuthorErrors Author)
registerAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  case maybeAuthor of
    Just _ -> return $ Left EmailAlreadyExists
    Nothing -> do
      registredAuthor <- createAuthor email' hashedPw
      return $ left OtherError registredAuthor

loginAction
  :: ManageAuthor m
  => Email
  -> Password 'Plain
  -> m (Either AuthorErrors Author)
loginAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  return $ case maybeAuthor of
    Nothing -> Left EmailNotExists
    Just authorInDb ->
      if password authorInDb /= hashedPw
      then Left EmailAndPasswordNotMatch
      else Right authorInDb
