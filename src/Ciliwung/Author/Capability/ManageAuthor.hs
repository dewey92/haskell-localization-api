module Ciliwung.Author.Capability.ManageAuthor where

import Ciliwung.AppM (AppM)
import Ciliwung.Author.Types (Email, Password, PasswordState(..))
import Ciliwung.Env (Env, dbConnection)
import Ciliwung.Database (Author(..), authorTable)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Database.Selda (query, insert_, def, select, restrict, literal, (!), (.==))
import Database.Selda.PostgreSQL (withPostgreSQL)

class Monad m => ManageAuthor m where
  createAuthor :: Email -> Password 'Hashed -> m (Either String Author)
  findAuthorByEmail :: Email -> m (Maybe Author)

instance ManageAuthor AppM where
  createAuthor email password = do
    conn <- asks dbConnection
    withPostgreSQL conn $
      insert_ authorTable [ Author def email password "" ]
    findAuthorByEmail email >>= \case
      Nothing -> return $ Left "Failed"
      Just newAuthor -> return $ Right newAuthor

  findAuthorByEmail email = do
    conn <- asks dbConnection
    withPostgreSQL conn $ do
      res <- query $ do
        authors <- select authorTable
        restrict $ (authors ! #email .== literal email)
        pure $ authors
      return $ case res of
        [a] -> Just a
        _   -> Nothing
