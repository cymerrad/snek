{-# LANGUAGE ImportQualifiedPost #-}

module Logic (gameEvent) where

import Brick.Types
  ( BrickEvent (..),
    EventM,
  )
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Mtl
import State
  ( Block (..),
    CharMatrix,
    CustomEvent (..),
    GameState,
    Snek (blocks, dirX, dirY),
    stGrid,
    stSnek,
    updateElement,
  )

gameEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
gameEvent e = do
  -- case e of
  --   -- WSAD
  --   VtyEvent (V.EvKey (V.KChar 'w') []) -> do
  --     stSnek %= \s -> s {dirX = 0, dirY = -1}
  --   VtyEvent (V.EvKey (V.KChar 's') []) -> do
  --     stSnek %= \s -> s {dirX = 0, dirY = 1}
  --   VtyEvent (V.EvKey (V.KChar 'a') []) -> do
  --     stSnek %= \s -> s {dirX = -1, dirY = 0}
  --   VtyEvent (V.EvKey (V.KChar 'd') []) -> do
  --     stSnek %= \s -> s {dirX = 1, dirY = 0}
  --   VtyEvent (V.EvKey (V.KChar 'q') []) -> do
  --     snek <- use stSnek
  --     liftIO $ BSL.appendFile "/tmp/dupa.txt" (encodePretty snek)
  --   _ -> return ()
  moveSnek e
  progress

progress :: EventM () GameState ()
progress = do
  updateSnek
  updateGrid

  -- snek <- use stSnek
  -- _ <- liftIO $ BSL.appendFile "/tmp/dupa.txt" (encodePretty snek)
  return ()

moveSnek :: BrickEvent () CustomEvent -> EventM () GameState ()
moveSnek (VtyEvent (V.EvKey (V.KChar c) []))
  | c == 'w' = stSnek %= \s -> s {dirX = 0, dirY = -1}
  | c == 's' = stSnek %= \s -> s {dirX = 0, dirY = 1}
  | c == 'a' = stSnek %= \s -> s {dirX = -1, dirY = 0}
  | c == 'd' = stSnek %= \s -> s {dirX = 1, dirY = 0}
  | otherwise = return ()
moveSnek _ = return ()

updateSnek :: EventM () GameState ()
updateSnek = do
  stSnek %= processSnek
  where
    processSnek snek =
      let (head_ :| body) = blocks snek
          newHead = Block (posX head_ + dirX snek) (posY head_ + dirY snek)
          newBody = Data.List.NonEmpty.init (head_ :| body)
       in snek {blocks = newHead :| newBody}

updateGrid :: EventM () GameState ()
updateGrid = do
  snek <- use stSnek
  stGrid %= (traversed . traversed .~ ' ')
  stGrid %= drawBlocks 'O' (blocks snek)
  -- TODO(raod) out of bounds and collision

drawBlocks :: Char -> NonEmpty Block -> CharMatrix -> CharMatrix
drawBlocks char blocks matrix =
  foldl (\m block -> updateElement (posY block) (posX block) char m) matrix blocks
