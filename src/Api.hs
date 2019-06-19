module Api where

import Control.Monad.Reader
import Servant
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)
import Types
import AppM
import Author.Apis (AuthorApis, authorApis)

-- | TODO: Add more routes
type Apis = AuthorApis -- :<|> TranslationApis

allApis = authorApis -- :<|> translationApis

proxyApis :: Proxy Apis
proxyApis = Proxy

-- | Unpack `AppM` into `Servant.Server.Handler`.
ntAppM :: Env -> AppM a -> Handler a
ntAppM env appM = runReaderT appM env

app :: Env -> Application
app env = serve proxyApis $ hoistServer proxyApis (ntAppM env) allApis

runServer :: IO ()
runServer = do
  let port = 5001
  -- init the Environment
  env <- useDefaultEnv
  putStrLn $ "Server running at port " <> show port
  run port $ app env
