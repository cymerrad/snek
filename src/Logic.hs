{-# LANGUAGE ImportQualifiedPost #-}

module Logic (gameEvent) where

import Brick.Types
  ( BrickEvent (..),
    EventM,
  )
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Mtl
import State

gameEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
gameEvent e =
  case e of
    AppEvent Counter -> progress
    VtyEvent (V.EvKey (V.KChar 'w') []) -> return ()
    _ -> return ()

progress :: EventM () GameState ()
progress = do
  updateSnek
  updateGrid

updateSnek :: EventM () GameState ()
updateSnek = do
  return ()

updateGrid :: EventM () GameState ()
updateGrid = do
  snek <- use stSnek
  stGrid %= drawBlocks 'O' (blocks snek)

drawBlocks :: Char -> [Block] -> CharMatrix -> CharMatrix
drawBlocks char blocks matrix =
  foldl (\m block -> updateElement (posY block) (posX block) char m) matrix blocks