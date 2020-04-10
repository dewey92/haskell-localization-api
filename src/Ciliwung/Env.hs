module Ciliwung.Env where

import Database.Selda.PostgreSQL (PGConnectInfo, on, auth)

data Env = Env
  { dbConnection :: PGConnectInfo
  }

useDefaultEnv :: IO Env
useDefaultEnv = do
  let dbConn = "ciliwung" `on` "localhost" `auth` ("root", "")
  return Env
    { dbConnection = dbConn
    }
