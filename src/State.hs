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
    oSpawns,
    bX,
    bY,
    sBlocks,
    sDirX,
    sDirY,
    sL,
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
  { _bX :: Int,
    _bY :: Int
  }
  deriving (Show, Eq)

instance ToJSON Block where
  toJSON (Block x y) = object ["x" .= x, "y" .= y]

instance FromJSON Block where
  parseJSON = withObject "Block" $ \v ->
    Block
      <$> v
      .: "x"
      <*> v
      .: "y"

data Snek = Snek
  { _sBlocks :: NonEmpty Block,
    _sDirX :: Int,
    _sDirY :: Int,
    _sL :: Int
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
  | Menu
  | Ended
  | Pause
  deriving (Show)

data Objective = Objective
  { _oLocations :: [Block],
    _oPoints :: Int,
    _oSpawns :: Int
  }

instance ToJSON Objective where
  toJSON (Objective locations points spawns) =
    object
      [ "locations" .= locations,
        "points" .= points,
        "spawns" .= spawns
      ]

instance FromJSON Objective where
  parseJSON = withObject "Objective" $ \v ->
    Objective
      <$> v
      .: "locations"
      <*> v
      .: "points"
      <*> v
      .: "spawns"

data GameState = GameState
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int,
    _stWindowSize :: (Int, Int),
    _stGrid :: CharMatrix,
    _stSnek :: Snek,
    _stStatus :: Status,
    _stObjective :: Objective
  }

instance ToJSON GameState where
  toJSON gs =
    object
      [ "lastEvent" .= show (_stLastBrickEvent gs),
        "windowSize" .= show (_stWindowSize gs),
        "status" .= show (_stStatus gs)
      ]

concat <$> mapM makeLenses [''Block, ''Snek, ''Objective, ''GameState]

initialState :: (Int, Int) -> GameState
initialState (width, height) =
  GameState
    { _stLastBrickEvent = Nothing,
      _stCounter = 0,
      _stWindowSize = (width, height),
      _stGrid = replicate height (replicate width ' '),
      _stSnek =
        Snek
          { _sBlocks =
              fromList
                [ Block 1 0,
                  Block 0 0,
                  Block 0 1
                ],
            _sDirX = 1,
            _sDirY = 0,
            _sL = 3
          },
      _stStatus = Running,
      _stObjective = Objective [] 0 10
    }