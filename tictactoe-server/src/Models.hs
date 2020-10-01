{-# LANGUAGE DeriveGeneric #-}

module Models
  ( Cell(..)
  , GameSession
  , Field
  )
where

import GHC.Generics
import Data.Aeson

type Field = [[Cell]]

data Cell = X | O | Empty
  deriving (Eq, Generic)

instance Show Cell where
  show X = "x"
  show O = "o"
  show Empty = " "

instance ToJSON Cell

data GameSession = GameSession
  { field :: Field
  }
