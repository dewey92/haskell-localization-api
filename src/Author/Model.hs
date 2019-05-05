{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- | Model is where all the business logics happen

module Author.Model where

import Control.Monad.Reader
import Control.Monad.Identity
import Types
import Author.Types
import Author.Capability.Database

registerAction
  :: (MonadAuthorDb m)
  => Email
  -> Password
  -> m (Either AuthorDbErrors AuthorEntity)
registerAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  case maybeAuthor of
    (Just _) -> return $ Left EmailAlreadyExists
    Nothing -> createAuthor email' password'

loginAction
  :: (MonadAuthorDb m)
  => Email
  -> Password
  -> m (Either AuthorDbErrors AuthorEntity)
loginAction email' password' = do
  maybeAuthor <- findAuthorByEmail email'
  return $ case maybeAuthor of
    Nothing -> Left EmailNotExists
    (Just authorInDb) ->
      if password authorInDb /= password'
      then Left EmailAndPasswordNotMatch
      else Right authorInDb

-- | Test
data Lol = Lol { _dbTest :: String }

newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

instance MonadAuthorDb TestM where
  createAuthor _ _ = return $ Left EmailAlreadyExists
  findAuthorByEmail _ = return Nothing

hasil :: TestM String
hasil =
  case mkPassword "hahaha" of
    Nothing -> return "hehe"
    (Just pw) -> loginAction (Email "aa@aa.com") pw >>= \case
        (Left _) -> return "Left"
        (Right _) -> return "Right"
