module Env where

import Database.MySQL.Simple as MySQL

data Env = Env
  { dbConnection :: MySQL.Connection
  }

useDefaultEnv :: IO Env
useDefaultEnv = do
  dbConn <- MySQL.connect defaultConnectInfo
    { MySQL.connectUser = "root"
    , MySQL.connectPassword = "root"
    , MySQL.connectDatabase = "loclz"
    , MySQL.connectPort = 3308
    }
  return Env
    { dbConnection = dbConn
    }
