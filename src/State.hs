{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module State
  ( GameState (..),
    CustomEvent (..),
    CharMatrix,
    Snek (..),
    Block (..),
    stLastBrickEvent,
    stCounter,
    stGrid,
    stSnek,
    updateElement,
    initialState,
  )
where

import Brick.Types
  ( BrickEvent (..),
  )
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data CustomEvent = Counter deriving (Show)

data Block = Block {posX :: Int, posY :: Int}

data Snek = Snek {blocks :: [Block], dirX :: Int, dirY :: Int}

type CharMatrix = [[Char]]

updateElement :: Int -> Int -> Char -> CharMatrix -> CharMatrix
updateElement row col newChar matrix =
  matrix & elementLens .~ newChar
  where
    elementLens = ix row . ix col

data GameState = GameState
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int,
    _stWidth :: Int,
    _stHeight :: Int,
    _stGrid :: CharMatrix,
    _stSnek :: Snek
  }

makeLenses ''GameState

initialState :: (Int, Int) -> GameState
initialState (width, height) =
  GameState
    { _stLastBrickEvent = Nothing,
      _stCounter = 0,
      _stWidth = width,
      _stHeight = height,
      _stGrid = replicate height (replicate width ' '),
      _stSnek = Snek {blocks = [Block 1 0, Block 0 0, Block 0 1, Block 0 2], dirX = 1, dirY = 1}
    }