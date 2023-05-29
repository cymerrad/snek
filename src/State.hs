{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module State
  ( GameState (..),
    CustomEvent (..),
    CharMatrix,
    Snek (..),
    Block (..),
    Status (..),
    stLastBrickEvent,
    stCounter,
    stGrid,
    stSnek,
    stStatus,
    stWindowSize,
    updateElement,
    initialState,
  )
where

import Brick.Types
  ( BrickEvent (..),
  )
import Data.Aeson
import Data.List.NonEmpty
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data CustomEvent = Counter deriving (Show)

data Block = Block
  { posX :: Int,
    posY :: Int
  }
  deriving (Show, Eq)

instance ToJSON Block where
  toJSON (Block x y) = object ["x" .= x, "y" .= y]

data Snek = Snek
  { blocks :: NonEmpty Block,
    dirX :: Int,
    dirY :: Int
  }
  deriving (Show)

instance ToJSON Snek where
  toJSON (Snek bs x y) = object ["bs" .= bs, "x" .= x, "y" .= y]

type Matrix a = [[a]]

type CharMatrix = Matrix Char

updateElement :: Int -> Int -> Char -> CharMatrix -> CharMatrix
updateElement row col newChar matrix =
  matrix & elementLens .~ newChar
  where
    elementLens = ix row . ix col

data Status
  = Running
  | Ended

data GameState = GameState
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int,
    -- _stWidth :: Int,
    -- _stHeight :: Int,
    _stWindowSize :: (Int, Int),
    _stGrid :: CharMatrix,
    _stSnek :: Snek,
    _stStatus :: Status
  }

makeLenses ''GameState

initialState :: (Int, Int) -> GameState
initialState (width, height) =
  GameState
    { _stLastBrickEvent = Nothing,
      _stCounter = 0,
      -- _stWidth = width,
      -- _stHeight = height,
      _stWindowSize = (width, height),
      _stGrid = replicate height (replicate width ' '),
      _stSnek =
        Snek
          { blocks =
              fromList
                [ Block 1 0,
                  Block 0 0,
                  Block 0 1,
                  Block 0 2
                ],
            dirX = 1,
            dirY = 0
          },
      _stStatus = Running
    }