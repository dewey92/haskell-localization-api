module Ciliwung.Api where

import Ciliwung.AppM (runAppM)
import Ciliwung.Author.Apis (AuthorApis, authorApis)
import Ciliwung.Env (Env, useDefaultEnv)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy(..), serve, hoistServer)

-- | TODO: Add more routes
type Apis = AuthorApis -- :<|> TranslationApis

proxyApis = Proxy @Apis

allApis = authorApis -- :<|> translationApis

app :: Env -> Application
app env = serve proxyApis $ hoistServer proxyApis (runAppM env) allApis

runServer :: IO ()
runServer = do
  let port = 5001
  -- init the Environment
  env <- useDefaultEnv
  putStrLn $ "Server running at port " <> show port
  run port $ app env
