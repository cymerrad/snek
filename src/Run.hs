{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Run (run) where

import Import hiding (threadDelay)
import Control.Concurrent (forkIO, threadDelay)
import Lens.Micro.Mtl
import Brick
import Brick.BChan (newBChan, writeBChan)
import Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Logic (gameEvent)
import State
import UI

-- TODO(rado) refactor
loop :: IO ()
loop = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Counter
    threadDelay 1000000

  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  terminalSize <- displayBounds $ outputIface initialVty

  -- TODO(rado) make the game play itself :D

  void $
    customMain
      initialVty
      buildVty
      (Just chan)
      theApp
      ( initialState
          ( regionWidth terminalSize - bordersHorizontal,
            regionHeight terminalSize - bordersVertical
          )
      )

appEvent :: BrickEvent () CustomEvent -> EventM () GameState ()
appEvent e = do
  stLastBrickEvent .= Just e

  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
    VtyEvent (V.EvKey (V.KChar 'p') []) -> do
      stStatus %= \case
        Pause -> Running
        Running -> Pause
        Ended -> Ended
        Menu -> Menu
    _ -> return ()

  status <- use stStatus
  case status of
    Running -> case e of
      AppEvent Counter -> do
        stCounter %= (+ 1)
        gameEvent e
      _ -> gameEvent e
    _ -> return ()

theApp :: App GameState CustomEvent ()
theApp =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr []
    }

run :: RIO RA ()
run = do
  -- logInfo "We're inside the application!"
  liftIO loop
