{-# LANGUAGE DataKinds #-}
-- | Model is where all the business logics happen

module Author.Model where

import Author.Types
  ( AuthorEntity(..)
  , AuthorErrors(..)
  , Email
  , Password
  , PasswordState(..)
  , hashPassword
  )
import Author.Capability.Database (ManageAuthor, createAuthor, findAuthorByEmail)
import Control.Arrow (left)

registerAction
  :: ManageAuthor m
  => Email
  -> Password 'Plain
  -> m (Either AuthorErrors AuthorEntity)
registerAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  case maybeAuthor of
    (Just _) -> return $ Left EmailAlreadyExists
    Nothing -> do
      registredAuthor <- createAuthor email' hashedPw
      return $ left OtherError registredAuthor

loginAction
  :: ManageAuthor m
  => Email
  -> Password 'Plain
  -> m (Either AuthorErrors AuthorEntity)
loginAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  return $ case maybeAuthor of
    Nothing -> Left EmailNotExists
    (Just authorInDb) ->
      if password authorInDb /= hashedPw
      then Left EmailAndPasswordNotMatch
      else Right authorInDb
