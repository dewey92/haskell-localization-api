{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
-- | Model is where all the business logics happen

module Author.Model where

import Control.Monad.Reader
import Control.Monad.Identity
import Types
import Author.Types
import Author.Capability.Database

-- | All kinds of database errors dealing with Author's domain
data AuthorDbErrors
  = EmailAlreadyExists
  | EmailAndPasswordNotMatch
  | EmailNotExists
  | OperationFailed

instance Show AuthorDbErrors where
  show EmailAlreadyExists = "Email already exists"
  show EmailAndPasswordNotMatch = "Email and password don't match"
  show EmailNotExists = "Email is not found in the system"
  show OperationFailed = "Operation failed"

registerAction
  :: (MonadAuthorDb m)
  => Email
  -> Password 'Raw
  -> m (Either AuthorDbErrors AuthorEntity)
registerAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  case maybeAuthor of
    (Just _) -> return $ Left EmailAlreadyExists
    Nothing -> createAuthor email' hashedPw >>= return . \case
      Nothing -> Left OperationFailed
      (Just r) -> Right r

loginAction
  :: (MonadAuthorDb m)
  => Email
  -> Password 'Raw
  -> m (Either AuthorDbErrors AuthorEntity)
loginAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  let hashedPw = hashPassword password'
  return $ case maybeAuthor of
    Nothing -> Left EmailNotExists
    (Just authorInDb) ->
      if password authorInDb /= hashedPw
      then Left EmailAndPasswordNotMatch
      else Right authorInDb
