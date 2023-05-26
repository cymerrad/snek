{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module State
  ( GameState (..),
    CustomEvent (..),
    stLastBrickEvent,
    stCounter,
  )
where

import Brick.Types
  ( BrickEvent (..),
  )
import Lens.Micro.TH (makeLenses)

data CustomEvent = Counter deriving (Show)

data GameState = GameState
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int
  }

makeLenses ''GameState