{-# LANGUAGE ImportQualifiedPost #-}

module Logic (gameEvent) where

import Brick.Types
  ( BrickEvent (..),
    EventM,
  )
import Control.Monad (when)
import Control.Monad.IO.Class
-- import Data.Aeson.Encode.Pretty
-- import Data.ByteString.Lazy qualified as BSL

import Data.List (intersect)
import Data.List.NonEmpty
  ( NonEmpty ((:|)),
    fromList,
    init,
    length,
    nub,
    toList,
  )
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Mtl
import State
  ( Block (..),
    CharMatrix,
    CustomEvent (..),
    GameState,
    Snek (..),
    Status (Ended),
    bX,
    bY,
    oLocations,
    oPoints,
    sBlocks,
    sDirX,
    sDirY,
    sL,
    stGrid,
    stObjective,
    stSnek,
    stStatus,
    stWindowSize,
    updateElement,
  )
import System.Random
import Prelude hiding (init, length)

gameEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
gameEvent e = do
  steerSnek e
  moveSnek
  checkCollision
  updateGrid
  drawGrid

steerSnek :: BrickEvent () CustomEvent -> EventM () GameState ()
steerSnek (VtyEvent (V.EvKey (V.KChar c) []))
  | c == 'w' = zoom stSnek $ do sDirX .= 0; sDirY .= -1
  | c == 's' = zoom stSnek $ do sDirX .= 0; sDirY .= 1
  | c == 'a' = zoom stSnek $ do sDirX .= -1; sDirY .= 0
  | c == 'd' = zoom stSnek $ do sDirX .= 1; sDirY .= 0
  | otherwise = return ()
steerSnek _ = return ()

moveSnek :: EventM () GameState ()
moveSnek = do
  stSnek %= processSnek
  where
    processSnek snek =
      let (head_ :| body) = snek ^. sBlocks
          newHead = Block (head_ ^. bX + snek ^. sDirX) (head_ ^. bY + snek ^. sDirY)
          newBody :: [Block]
          newBody =
            if length (snek ^. sBlocks) < snek ^. sL
              then toList (head_ :| body)
              else init (head_ :| body)
       in snek {_sBlocks = newHead :| newBody}

checkCollision :: EventM () GameState ()
checkCollision = do
  (wW, wH) <- use stWindowSize
  snek <- use stSnek
  fud <- use (stObjective . oLocations)
  let ((Block headX headY) :| _) = snek ^. sBlocks

  let outOfBounds = headX < 0 || headX >= wW || headY < 0 || headY >= wH
  let hitItself = hasDuplicates (snek ^. sBlocks)

  let snekFudIntersection = fud `intersect` toList (snek ^. sBlocks)
  let hitFud = not . null $ snekFudIntersection

  when (outOfBounds || hitItself) gameOver

  when hitFud $ do
    let foundFud = head snekFudIntersection
    stObjective . oLocations %= filter (/= foundFud)
    stObjective . oPoints += 1
    stSnek . sL += 1
  where
    hasDuplicates xs = length xs /= length (nub xs)

updateGrid :: EventM () GameState ()
updateGrid = do
  (wW, wH) <- use stWindowSize
  existingLocations <- use (stObjective . oLocations)
  snek <- use stSnek
  let snakeLocations = toList $ snek ^. sBlocks
  filledLocations <-
    liftIO $
      generateNonOverlappingCoordinates
        (wW - 1)
        (wH - 1)
        snakeLocations
        existingLocations
        (5 - myLength existingLocations)
  (stObjective . oLocations) .= filledLocations
  where
    myLength = foldr (\_ acc -> acc + 1) 0

generateNonOverlappingCoordinates :: Int -> Int -> [Block] -> [Block] -> Int -> IO [Block]
generateNonOverlappingCoordinates maxX maxY snakeLocations = go
  where
    go :: [Block] -> Int -> IO [Block]
    go acc 0 = return acc
    go acc count = do
      newBlock <- Block <$> randomRIO (0, maxX) <*> randomRIO (0, maxY)
      if (newBlock `elem` acc) || (newBlock `elem` snakeLocations)
        then go acc count
        else go (newBlock : acc) (count - 1)

drawGrid :: EventM () GameState ()
drawGrid = do
  snek <- use stSnek
  fud <- use (stObjective . oLocations)

  stGrid %= (traversed . traversed .~ ' ')
  stGrid %= drawBlocks 'O' (snek ^. sBlocks)
  stGrid %= drawBlocks '+' (fromList fud)

drawBlocks :: Char -> NonEmpty Block -> CharMatrix -> CharMatrix
drawBlocks char blocks matrix =
  foldl (\m block -> updateElement (block ^. bY) (block ^. bX) char m) matrix blocks

gameOver :: EventM () GameState ()
gameOver = do
  stStatus .= Ended
  return ()
