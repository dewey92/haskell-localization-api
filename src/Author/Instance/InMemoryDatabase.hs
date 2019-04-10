{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Author.Instance.InMemoryDatabase where

import Prelude hiding (lookup)
import Data.Cache
import Control.Monad.Except (MonadIO, liftIO)
import Author.Capability.Database
import Author.Model
import System.IO.Unsafe

newtype InMemory a = InMemory { unInMemory :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | In memory database instance.
-- | It will be used as a singleton by the functions below
{-# NOINLINE cacheInstance #-}
cacheInstance :: Cache Int AuthorInDb
cacheInstance = unsafePerformIO $ newCache Nothing

-- | Populate initial data for testing purpose. Need to be called manually
-- | in `Main.hs` or in your other entry file
initCache = insert cacheInstance 1 newAuthor
  where
    newAuthor = AuthorInDb
      { author_id = 1
      , email = Email "dewey992@gmail.com"
      , password = "password1234"
      , fullname = "Jihad Dzikri Waspada"
      , created_at = ""
      , updated_at = ""
      }

instance AuthorMonadDb InMemory where
  -- register
  register author password = do
    maybeAuthor <- findAuthorByEmail (authorEmail author)
    case maybeAuthor of
      (Just _) -> return $ Left EmailAlreadyExists
      Nothing -> do
        newAuthorInDb <- liftIO $ do
          newId <- (+1) <$> size cacheInstance
          let newAuthor = AuthorInDb
                { author_id = newId
                , email = authorEmail author
                , password = showPassword password
                , fullname = ""
                , created_at = ""
                , updated_at = ""
                }
          insert cacheInstance newId newAuthor
          return newAuthor
        return $ Right newAuthorInDb

  -- login
  login emailInput passwordInput = do
    maybeAuthor <- findAuthorByEmail emailInput
    case maybeAuthor of
      Nothing -> return $ Left EmailNotExists
      (Just authorInDb) ->
        if password authorInDb /= showPassword passwordInput
        then return $ Left EmailAndPasswordNotMatch
        else return $ Right authorInDb

  -- findAuthorByEmail
  findAuthorByEmail emailInput = liftIO $ do
    allIds <- keys cacheInstance
    let
      search [] = return Nothing
      search (currId:ids) = do
        author <- lookup cacheInstance currId
        case author of
          (Just a) -> if email a == emailInput
            then return $ Just a
            else search ids
          Nothing -> search ids
    search allIds
