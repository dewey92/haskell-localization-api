{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module AppM where

import Control.Monad.Reader
import Control.Monad.Except
import Servant.Server
import Database.MySQL.Simple
import Data.Cache
import Author.Capability.Database
import Author.Capability.InMemoryDatabase

-- | TODO: Change Env
data Env = Env
  { authorDbConn :: Cache Int AuthorEntity
  , ganteng :: String
  , dbConn :: Connection
  }

useDefaultEnv :: IO Env
useDefaultEnv = do
  cacheInstance <- newCache Nothing
  dbConn <- connect defaultConnectInfo
    { connectUser = "root"
    , connectPassword = "root"
    , connectDatabase = "loclz"
    , connectPort = 3308
    }
  return Env
    { authorDbConn = cacheInstance
    , dbConn = dbConn
    , ganteng = "Jihad"
    }

type AppM = ReaderT Env Handler

-- | Grant `AppM` the capabilities to access Author Database. I'm using -XTypeApplications
-- | so that the implementation can be extracted to another file instead of
-- | polluting in this file. It would be too long and will become unreadable if put here
-- | since the purpose of this module is just to define all capabilities of `AppM`
instance AuthorMonadDb AppM where
  type Conn AppM = (Cache Int AuthorEntity)
  register conn email password =
    liftIO $ unInMemory (register @InMemory conn email password)
  login conn email password =
    liftIO $ unInMemory (login @InMemory conn email password)
  findAuthorByEmail conn email =
    liftIO $ unInMemory (findAuthorByEmail @InMemory conn email)
