{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Author.Capability.MySQLDatabase where

import Control.Monad.IO.Class
import Data.Maybe
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Param)
import Database.MySQL.Simple.Result (Result, convert)
import Database.MySQL.Simple.QueryResults (QueryResults, convertResults, convertError)
import Author.Capability.Database
import Author.Model

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

-- | AuthorDB using MySQL implementation
newtype AuthorMySQL a = AuthorMySQL (IO a) deriving
  (Functor, Applicative, Monad, MonadIO)

instance AuthorMonadDb AuthorMySQL where
  type Conn AuthorMySQL = Connection

  register conn emailInput passwordInput = do
    maybeAuthor <- findAuthorByEmail conn emailInput
    case maybeAuthor of
      (Just _) -> return $ Left EmailAlreadyExists
      Nothing -> do
        liftIO $ execute
          conn
          "INSERT INTO users (email, password) VALUES (?,?)"
          (emailInput, showPassword passwordInput)
        maybeNewAuthor <- findAuthorByEmail conn emailInput
        return $ case maybeNewAuthor of
          Nothing -> Left OperationFailed
          (Just newAuthor) -> Right newAuthor

  -- login
  login conn emailInput passwordInput = do
    maybeAuthor <- findAuthorByEmail conn emailInput
    return $ case maybeAuthor of
      Nothing -> Left EmailNotExists
      (Just authorInDb) ->
        if password authorInDb /= passwordInput
        then Left EmailAndPasswordNotMatch
        else Right authorInDb

  -- findAuthorByEmail
  findAuthorByEmail conn emailInput = do
    author <- liftIO $ query conn "SELECT * FROM users WHERE email = ?" (Only emailInput)
    return $ case author of
      [a] -> Just a
      _ -> Nothing
