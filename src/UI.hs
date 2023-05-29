{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}

module UI (loop) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
-- import Control.Lens ((~.))
import Lens.Micro ((^.))
import Lens.Micro.Mtl

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Brick.AttrMap
  ( attrMap,
  )
import Brick.BChan
import Brick.Main
  ( App (..),
    customMain,
    halt,
    showFirstCursor,
  )
import Brick.Types
  ( BrickEvent (..),
    EventM,
    Widget,
  )
import Brick.Widgets.Core
  ( str,
    (<=>),
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Graphics.Vty (Output (displayBounds), outputIface, regionHeight, regionWidth)
import Graphics.Vty qualified as V
import Logic (gameEvent)
import State

renderMatrix :: CharMatrix -> [Widget ()]
renderMatrix = map str

drawUI :: GameState -> [Widget ()]
drawUI st = case st ^. stStatus of
  Running -> [a]
    where
      grid = st ^. stGrid
      a0 : as = renderMatrix grid
      a = foldl (<=>) a0 as
  Ended -> [(str $ "You suck :)")]

    -- a =
    --   (str $ "Last event: " <> (show $ st ^. stLastBrickEvent))
    --     <=> (str $ "Counter value is: " <> (show $ st ^. stCounter))
    --     <=> (str $ "Snek: " <> (BSL.unpack . encodePretty $ st ^. stSnek))

appEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
appEvent e = do
  case e of
    VtyEvent _ -> stLastBrickEvent .= (Just e)
    _ -> return ()

  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    AppEvent Counter -> do
      stCounter %= (+ 1)
      stLastBrickEvent .= (Just e)
      gameEvent e
    _ -> do
      stLastBrickEvent .= (Just e)
      gameEvent e

theApp :: App GameState CustomEvent ()
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr []
    }

loop :: IO ()
loop = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Counter
    threadDelay 1000000

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  terminalSize <- displayBounds $ outputIface initialVty

  -- _ <- liftIO $ appendFile "/tmp/dupa.txt" ("Viewport size: " ++ show terminalSize)

  void $ customMain initialVty buildVty (Just chan) theApp (initialState (regionWidth terminalSize, regionHeight terminalSize))
