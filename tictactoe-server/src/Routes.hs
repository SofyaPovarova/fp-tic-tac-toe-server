{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Control.Monad.State
import Data.Aeson
import GHC.Conc (STM)
import GHC.Generics (Generic)
import Game
import Models
import Servant
import StmContainers.Map (Map)
import Data.UUID
import Data.UUID.V4

data AppState = AppState
  { registeredUsers :: STM (Map String String)
  }

type AppM = StateT AppState Handler

type STMServer a = ServerT a AppM

data User = User {name :: String, email :: String}
  deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance FromJSON User

data Login = Login {username :: String, password :: String}
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login

instance FromJSON Login

type SessionResponse a = (Headers '[Header "Session" String] a)

type API =
  "startGame"
    :> QueryParam "fieldSize" Int
    :> QueryParam "role" Cell
    :> Post '[JSON] (SessionResponse Field)

server :: AppState -> Server API
server env = hoistServer api transform server'
  where
    transform :: AppM a -> Handler a
    transform appM = evalStateT appM env

api :: Proxy API
api = Proxy

server' :: STMServer API
server' =
  startGameHandler


startGameHandler :: Maybe Int -> Maybe Cell -> AppM (SessionResponse Field)
startGameHandler fieldSizeParam roleParam =
  case (fieldSizeParam, roleParam) of
    (Just fieldSize, Just role) -> do
      sessionId <- liftIO generateRandomSessionId
      return $ addHeader sessionId $ emptyField fieldSize
    _ -> throwError (err400 {errBody = "Field size and role should be specified"})

generateRandomSessionId :: IO String
generateRandomSessionId = (nextRandom :: IO UUID) >>= return . show