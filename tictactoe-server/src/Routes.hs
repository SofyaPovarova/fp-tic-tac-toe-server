{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Routes where

import Prelude hiding (lookup)

import Control.Monad.State
import GHC.Conc (TVar, readTVar, atomically)
import GHC.Generics (Generic)
import Game
import Models
import Servant
import StmContainers.Map (Map, insert, lookup)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text (Text, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Random (randomIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Lens.Micro ((^.))

data AppState = AppState
  { appSessions :: TVar (Map String GameSession)
  }
  
data PlayerRoleParam = PlayerRoleParam Cell | PlayerRoleRandom
  deriving (Eq, Generic)
  
instance FromHttpApiData PlayerRoleParam where
  parseQueryParam :: Text -> Either Text PlayerRoleParam
  parseQueryParam text =
    case (unpack text) of
      "x" -> Right $ PlayerRoleParam X
      "o" -> Right $ PlayerRoleParam O
      "random" -> Right PlayerRoleRandom
      s -> Left $ pack $ "Unknown parameter: " ++ s

type AppM = ReaderT AppState Handler

type STMServer a = ServerT a AppM

type SessionResponse a = (Headers '[Header "Session" String] a)

type API =
  "startGame"
    :> QueryParam "fieldSize" Int
    :> QueryParam "role" PlayerRoleParam
    :> Post '[JSON] (SessionResponse GameSession)
  :<|> "move"
    :> Header "Session" String
    :> QueryParam "x" Int
    :> QueryParam "y" Int
    :> Post '[JSON] (SessionResponse GameSession)
 :<|> "session"
    :> Header "Session" String
    :> Get '[JSON] (SessionResponse GameSession)

server :: AppState -> Server API
server env = hoistServer api transform server'
  where
    transform :: AppM a -> Handler a
    transform appM = runReaderT appM env

api :: Proxy API
api = Proxy

server' :: STMServer API
server' =
  startGameHandler :<|> moveHandler :<|> sessionHandler

startGameHandler :: Maybe Int -> Maybe PlayerRoleParam -> AppM (SessionResponse GameSession)
startGameHandler mFieldSize mRoleParam =
  case (mFieldSize, mRoleParam) of
    (Just fieldSize, Just roleParam) -> do
      sessionId <- liftIO generateRandomSessionId
      playerRole <- liftIO $ roleParamToRole roleParam
      case (emptyField fieldSize playerRole) of
        Left err -> throwError $ err400 {errBody = BS.pack err}
        Right field -> do
          let gameSession = 
                GameSession
                  { _gsField = field
                  , _gsPlayerRole = playerRole
                  }
          _ <- getSessionMap >>= liftIO . atomically . insert gameSession sessionId
          returnWithSession sessionId $ gameSession
    _ -> throwError $ err400 {errBody = "Field size and role should be specified"}

generateRandomSessionId :: IO String
generateRandomSessionId = (nextRandom :: IO UUID) >>= return . show

roleParamToRole :: PlayerRoleParam -> IO Cell
roleParamToRole = 
  \case
    PlayerRoleParam cell -> return cell
    PlayerRoleRandom -> do
      randomNumber <- randomIO :: IO Double
      return $ if randomNumber <= 0.5 then X else O
    
moveHandler :: 
  Maybe String -> 
  Maybe Int -> 
  Maybe Int -> 
  AppM (SessionResponse GameSession)
moveHandler mSessionId mX mY =
  case (mSessionId, mX, mY) of 
    (Nothing, _, _) -> throwError err404
    (Just sessionId, Just x, Just y) -> do
      gameSession <- requireSession sessionId
      
      let mPlayerMoveField = move x y (gameSession^.gsPlayerRole) (gameSession^.gsField)
      playerMoveField <- 
        case mPlayerMoveField of
          Left CellNotEmptyError -> throwError $ err400 {errBody = "Cell is not empty"}
          Left OutOfBoundsError -> throwError $ err400 {errBody = "Attempt to move out of bounds"}
          Right newField -> return newField
      
      let mComputerMoveField = undefined
      
      undefined
  
sessionHandler :: Maybe String -> AppM (SessionResponse GameSession)
sessionHandler =
  \case
    Just sessionId -> requireSession sessionId >>= returnWithSession sessionId
    _ -> throwError err404  


getSessionMap :: AppM (Map String GameSession)
getSessionMap = asks appSessions >>= liftIO . atomically . readTVar
  
getSession :: String -> AppM (Maybe GameSession)
getSession sessionId = getSessionMap >>= liftIO . atomically . lookup sessionId

requireSession :: String -> AppM GameSession
requireSession sessionId = do
  mSession <- getSession sessionId
  case mSession of
    Just session -> return session
    Nothing -> throwError $ err401 {errBody = "No session found by provided id"}

returnWithSession :: String -> a -> AppM (SessionResponse a)
returnWithSession sessionId = return . (addHeader sessionId)
