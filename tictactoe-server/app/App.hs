{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import Network.Wai.Handler.Warp
import StmContainers.Map (new, Map)
import GHC.Conc (STM)
import Routes
import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = do
  let port = 8081
 
  let initialState = AppState newMap
      
  putStrLn $ "========\nStarting server on localhost:" ++ show port
  run port $ app initialState
  
nt :: AppState -> AppM a -> Handler a
nt state appM = evalStateT appM state

app :: AppState -> Application
app state = serve api $ server state
  
newMap :: STM (Map String String)
newMap = new