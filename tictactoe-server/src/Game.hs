module Game where

import Models

emptyField :: Int -> Field
emptyField size
  | size < 3 = undefined
  | otherwise = replicate size $ replicate size Empty
