module Types where

import Database.MySQL.Simple

data Env = Env
  { dbConnection :: Connection
  }

useDefaultEnv :: IO Env
useDefaultEnv = do
  dbConn <- connect defaultConnectInfo
    { connectUser = "root"
    , connectPassword = "root"
    , connectDatabase = "loclz"
    , connectPort = 3308
    }
  return Env
    { dbConnection = dbConn
    }
