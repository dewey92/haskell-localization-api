module Api where

import AppM (runAppM)
import Author.Apis (AuthorApis, authorApis)
import Env (Env, useDefaultEnv)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy(..), serve, hoistServer)

-- | TODO: Add more routes
type Apis = AuthorApis -- :<|> TranslationApis

allApis = authorApis -- :<|> translationApis

proxyApis :: Proxy Apis
proxyApis = Proxy

app :: Env -> Application
app env = serve proxyApis $ hoistServer proxyApis (runAppM env) allApis

runServer :: IO ()
runServer = do
  let port = 5001
  -- init the Environment
  env <- useDefaultEnv
  putStrLn $ "Server running at port " <> show port
  run port $ app env
