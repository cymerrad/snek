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
    Objective (..),
    stLastBrickEvent,
    stCounter,
    stGrid,
    stSnek,
    stStatus,
    stObjective,
    stWindowSize,
    oLocations,
    oPoints,
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
    dirY :: Int,
    l :: Int
  }
  deriving (Show)

instance ToJSON Snek where
  toJSON (Snek (h :| _) x y _) = object ["head" .= h, "x" .= x, "y" .= y]

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

data Objective = Objective
  { _oLocations :: [Block],
    _oPoints :: Int
  }

data GameState = GameState
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int,
    _stWindowSize :: (Int, Int),
    _stGrid :: CharMatrix,
    _stSnek :: Snek,
    _stStatus :: Status,
    _stObjective :: Objective
  }

concat <$> mapM makeLenses [''Objective, ''GameState]

initialState :: (Int, Int) -> GameState
initialState (width, height) =
  GameState
    { _stLastBrickEvent = Nothing,
      _stCounter = 0,
      _stWindowSize = (width, height),
      _stGrid = replicate height (replicate width ' '),
      _stSnek =
        Snek
          { blocks =
              fromList
                [ Block 1 0,
                  Block 0 0,
                  Block 0 1
                ],
            dirX = 1,
            dirY = 0,
            l = 4
          },
      _stStatus = Running,
      _stObjective = Objective [] 0
    }