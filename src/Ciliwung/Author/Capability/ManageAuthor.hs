{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Ciliwung.Author.Capability.ManageAuthor where

import Ciliwung.AppM (AppM)
import Ciliwung.Author.Types (Email, AuthorEntity(..), Password, PasswordState(..))
import Ciliwung.Env (Env, dbConnection)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Database.MySQL.Simple (Connection, Only(..), execute, query)

class Monad m => ManageAuthor m where
  createAuthor :: Email -> Password 'Hashed -> m (Either String AuthorEntity)
  findAuthorByEmail :: Email -> m (Maybe AuthorEntity)

instance ManageAuthor AppM where
  createAuthor emailInput passwordInput = runDB $ \conn -> do
    _ <- liftIO $ execute
      conn
      "INSERT INTO users (email, password) VALUES (?,?)"
      (emailInput, passwordInput)
    findAuthorByEmail emailInput >>= \case
      Nothing -> return $ Left "Failed"
      (Just newAuthor) -> return $ Right newAuthor
  findAuthorByEmail emailInput = runDB $ \conn -> do
    author <- liftIO
      $ query conn "SELECT * FROM users WHERE email = ?" (Only emailInput)
    return $ case author of
      [a] -> Just a
      _   -> Nothing

-- | Helper function to inject the connection
runDB :: MonadReader Env m => (Connection -> m r) -> m r
runDB callback = do
  conn <- asks dbConnection
  callback conn
