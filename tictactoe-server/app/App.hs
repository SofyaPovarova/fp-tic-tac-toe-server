module Main where

import Servant
import Network.Wai.Handler.Warp
import Routes
import Game

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "========\nStarting server on localhost:" ++ show port
  run port app

app :: Application
app = serve gameAPI server

gameAPI :: Proxy GameAPI
gameAPI = Proxy

server :: Server GameAPI
server = return (emptyField 3)
