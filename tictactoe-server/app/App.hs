{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import Routes
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings, generateKey, JWT)

main :: IO ()
main = do
  let port = 8081

  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
  let cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      
  putStrLn $ "========\nStarting server on localhost:" ++ show port
  run port $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)
  
api :: Proxy (API '[JWT])
api = Proxy
