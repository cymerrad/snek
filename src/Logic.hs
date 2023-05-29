{-# LANGUAGE ImportQualifiedPost #-}

module Logic (gameEvent) where

import Brick.Types
  ( BrickEvent (..),
    EventM,
  )
import Control.Monad (when)
-- import Control.Monad.IO.Class
-- import Data.Aeson.Encode.Pretty
-- import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty ((:|)), init, length, nub)
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Mtl
import State
  ( Block (..),
    CharMatrix,
    CustomEvent (..),
    GameState,
    Snek (blocks, dirX, dirY),
    Status (Ended),
    stGrid,
    stSnek,
    stStatus,
    stWindowSize,
    updateElement,
  )
import Prelude hiding (init, length)

gameEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
gameEvent e = do
  steerSnek e
  updateSnek
  checkCollision
  updateGrid

steerSnek :: BrickEvent () CustomEvent -> EventM () GameState ()
steerSnek (VtyEvent (V.EvKey (V.KChar c) []))
  | c == 'w' = stSnek %= \s -> s {dirX = 0, dirY = -1}
  | c == 's' = stSnek %= \s -> s {dirX = 0, dirY = 1}
  | c == 'a' = stSnek %= \s -> s {dirX = -1, dirY = 0}
  | c == 'd' = stSnek %= \s -> s {dirX = 1, dirY = 0}
  | otherwise = return ()
steerSnek _ = return ()

updateSnek :: EventM () GameState ()
updateSnek = do
  stSnek %= processSnek
  where
    processSnek snek =
      let (head_ :| body) = blocks snek
          newHead = Block (posX head_ + dirX snek) (posY head_ + dirY snek)
          newBody = init (head_ :| body)
       in snek {blocks = newHead :| newBody}

checkCollision :: EventM () GameState ()
checkCollision = do
  (wW, wH) <- use stWindowSize
  snek <- use stSnek
  let ((Block headX headY) :| _) = blocks snek

  let outOfBounds = headX < 0 || headX >= wW || headY < 0 || headY >= wH
  let hitItself = hasDuplicates (blocks snek)

  when (outOfBounds || hitItself) gameOver
  where
    hasDuplicates xs = length xs /= length (nub xs)

updateGrid :: EventM () GameState ()
updateGrid = do
  snek <- use stSnek
  stGrid %= (traversed . traversed .~ ' ')
  stGrid %= drawBlocks 'O' (blocks snek)

drawBlocks :: Char -> NonEmpty Block -> CharMatrix -> CharMatrix
drawBlocks char blocks matrix =
  foldl (\m block -> updateElement (posY block) (posX block) char m) matrix blocks

gameOver :: EventM () GameState ()
gameOver = do
  stStatus .= Ended
  return ()