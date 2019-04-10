module Api where

import Servant
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)

import AppM
import Author.Apis (AuthorApis, authorApis)
import Control.Monad.Reader

-- | TODO: Add more routes
type Apis = AuthorApis

allApis = authorApis

proxyApis :: Proxy Apis
proxyApis = Proxy

-- | Transform `AppM` into `Wai` compliant.
-- | So-called Natural Transformation, thus shortened to `nt`
ntAppM :: Env -> AppM a -> Handler a
ntAppM env appM = runReaderT (unAppM appM) env

app :: Env -> Application
app env = serve proxyApis $ hoistServer proxyApis (ntAppM env) allApis

runServer :: IO ()
runServer = do
  let port = 5001
  let env = "Jihad"
  putStrLn $ "Server running at port " <> show port
  run port $ app env
