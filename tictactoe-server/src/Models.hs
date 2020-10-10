{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Lens.Micro.TH
import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map


data Cell = X | O
  deriving (Eq, Generic)

instance Show Cell where
  show X = "x"
  show O = "o"

instance ToJSON Cell


data Field = Field
  { _fieldCells :: Map.Map (Int, Int) Cell
  , _fieldSize :: Int
  }
  
makeLenses ''Field

instance ToJSON Field where
  toJSON Field{..} = object
    [ "field" .= _fieldCells
    , "size" .= _fieldSize
    ] 


data GameSession = GameSession
  { _gsField :: Field
  , _gsPlayerRole :: Cell
  } deriving (Generic)


instance ToJSON GameSession where
  toJSON GameSession{..} = object
    [ "field" .= _gsField
    , "playerRole" .= _gsPlayerRole
    ]
    
makeLenses ''GameSession

