{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Models
  ( Cell(..)
  , GameSession
  , Field
  )
where

import GHC.Generics
import Data.Aeson
import Servant
import Data.Text

type Field = [[Cell]]

data Cell = X | O | Empty
  deriving (Eq, Generic)

instance FromHttpApiData Cell where
  parseQueryParam :: Text -> Either Text Cell
  parseQueryParam text =
    case (unpack text) of
      "x" -> Right X
      "o" -> Right O
      "random" -> Right X
      s -> Left $ pack $ "Unknown parameter: " ++ s

instance Show Cell where
  show X = "x"
  show O = "o"
  show Empty = " "

instance ToJSON Cell

data GameSession = GameSession
  { field :: Field
  }
