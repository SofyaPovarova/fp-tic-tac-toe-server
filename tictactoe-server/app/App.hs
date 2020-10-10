{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import StmContainers.Map (Map, newIO)
import GHC.Conc (newTVar, atomically)
import Routes
import Models

main :: IO ()
main = do
  let port = 8081
 
  sessionMap <- newMap >>= atomically . newTVar
  let initialState = AppState sessionMap
      
  putStrLn $ "========\nStarting server on localhost:" ++ show port
  run port $ app initialState
  
app :: AppState -> Application
app state = serve api $ server state
  
newMap :: IO (Map String GameSession)
newMap = newIO