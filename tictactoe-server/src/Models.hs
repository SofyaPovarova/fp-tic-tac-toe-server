{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Models where

import qualified Data.Map as Map

import Lens.Micro.TH
import GHC.Generics
import Data.Aeson
import Data.Text (pack)

data Cell = X | O
  deriving (Eq, Generic)

instance Show Cell where
  show = \case
    X -> "x"
    O -> "o"

instance ToJSON Cell where
  toJSON = Data.Aeson.String . pack . show

data GameResult = CrossesWin | NoughtsWin | Draw
  deriving (Eq, Generic)

instance Show GameResult where
    show = \case
      CrossesWin -> "x"
      NoughtsWin -> "o"
      Draw -> "draw"

instance ToJSON GameResult where
  toJSON = Data.Aeson.String . pack . show

toGameResult :: Cell -> GameResult
toGameResult =
  \case
    X -> CrossesWin
    O -> NoughtsWin

data Field = Field
  { _fieldCells :: Map.Map (Int, Int) Cell
  , _fieldSize :: Int
  }
  
makeLenses ''Field

instance ToJSON Field where
  toJSON field = object
    [ "cells" .= _fieldCells field
    , "size" .= _fieldSize field
    ]  

data GameSession = GameSession
  { _gsField :: Field
  , _gsPlayerRole :: Cell
  , _gsGameResult :: Maybe GameResult
  , _gsWinLineLength :: Int
  } deriving (Generic)


instance ToJSON GameSession where
  toJSON GameSession{..} = object
    [ "field" .= _gsField
    , "playerRole" .= _gsPlayerRole
    , "gameResult" .= _gsGameResult
    ]
    
makeLenses ''GameSession

