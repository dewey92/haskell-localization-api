{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Author.Capability.MySQLDatabase
  ( createAuthor
  , findAuthorByEmail
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Param)
import Database.MySQL.Simple.Result (Result, convert)
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults, convertError)
import Types
import Author.Capability.Database (AuthorDbErrors(..), AuthorEntity(..))
import Author.Types

-- | Make Email type an instance of `Param` and `Result` so that it could be
-- | converted from arbitrary DB record back to `Email` type and viceverca
deriving instance Param Email
deriving instance Result Email

instance QueryResults AuthorEntity where
  convertResults
    [f_authorId, f_email, f_pw, f_fullname, f_createdAt, f_updatedAt] -- `f` stands for `field`
    [v_id, v_email, v_pw, v_fullname, v_createdAt, v_updatedAt]
    = MkAuthorEntity author_id' email' password' fullname' created_at' updated_at'
    where
      !author_id' = convert f_authorId v_id
      !email' = convert f_email v_email
      !password' = fromJust <$> mkPassword $ convert f_pw v_pw -- Safe to coerce `Password` from DB
      !fullname' = convert f_fullname v_fullname
      !created_at' = convert f_createdAt v_createdAt
      !updated_at' = convert f_updatedAt v_updatedAt
  convertResults fs vs = convertError fs vs 6

 -- TODO: should commit in one transaction
createAuthor
  :: (MonadReader Env m, MonadIO m)
  => Email
  -> Password
  -> m (Either AuthorDbErrors AuthorEntity)
createAuthor emailInput passwordInput = runDB $ \conn -> do
  liftIO $ execute
    conn
    "INSERT INTO users (email, password) VALUES (?,?)"
    (emailInput, showPassword passwordInput)
  findAuthorByEmail emailInput >>= \case
    Nothing -> return $ Left OperationFailed
    (Just newAuthor) -> return $ Right newAuthor

findAuthorByEmail
  :: (MonadReader Env m, MonadIO m)
  => Email
  -> m (Maybe AuthorEntity)
findAuthorByEmail emailInput = runDB $ \conn -> do
  author <- liftIO $ query conn "SELECT * FROM users WHERE email = ?" (Only emailInput)
  return $ case author of
    [a] -> Just a
    _ -> Nothing

-- | Inject the connection
-- TODO: May fork the connection ??
runDB :: (MonadReader Env m) => (Connection -> m r) -> m r
runDB callback = do
  conn <- asks dbConnection
  callback conn
