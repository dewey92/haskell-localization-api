{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Author.Capability.InMemoryDatabase where

import Prelude hiding (lookup)
import Data.Cache
import Control.Monad.Except (MonadIO, liftIO)
import Author.Capability.Database
import Author.Model
import System.IO.Unsafe

newtype InMemory a = InMemory { unInMemory :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance AuthorMonadDb InMemory where
  type Conn InMemory = (Cache Int AuthorEntity)
  -- register
  register conn emailInput passwordInput = do
    maybeAuthor <- findAuthorByEmail conn emailInput
    case maybeAuthor of
      (Just _) -> return $ Left EmailAlreadyExists
      Nothing -> do
        newAuthorInDb <- liftIO $ do
          newId <- (+1) <$> size conn
          let newAuthor = MkAuthorEntity
                { author_id = newId
                , email = emailInput
                , password = passwordInput
                , fullname = ""
                , created_at = ""
                , updated_at = ""
                }
          insert conn newId newAuthor
          return newAuthor
        return $ Right newAuthorInDb

  -- login
  login conn emailInput passwordInput = do
    maybeAuthor <- findAuthorByEmail conn emailInput
    case maybeAuthor of
      Nothing -> return $ Left EmailNotExists
      (Just authorInDb) ->
        if password authorInDb /= passwordInput
        then return $ Left EmailAndPasswordNotMatch
        else return $ Right authorInDb

  -- findAuthorByEmail
  findAuthorByEmail conn emailInput = liftIO $ do
    allIds <- keys conn
    let
      search [] = return Nothing
      search (currId:ids) = do
        author <- lookup conn currId
        case author of
          (Just a) -> if email a == emailInput
            then return $ Just a
            else search ids
          Nothing -> search ids
    search allIds
