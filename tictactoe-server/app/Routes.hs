{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Models
import Servant

type GameAPI = "field" :> Get '[JSON] Field
