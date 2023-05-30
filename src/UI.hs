{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Graphics.Vty (Output (displayBounds), outputIface, regionHeight, regionWidth)
import Graphics.Vty qualified as V
import Logic (gameEvent)
import State
import Text.Wrap (defaultWrapSettings, preserveIndentation)
import Data.Aeson (encode)

renderMatrix :: CharMatrix -> [Widget ()]
renderMatrix = map str

drawBoard :: GameState -> [Widget ()]
drawBoard st = case st ^. stStatus of
  Running -> [a]
    where
      grid = st ^. stGrid
      a0 : as = renderMatrix grid
      a = foldl (<=>) a0 as
  Ended -> [center . str $ "Game over"]
  Pause -> [t, fill ' ']
    where
      settings = defaultWrapSettings {preserveIndentation = True}
      t =
        strWrapWith settings $
          "Press (P) to unpause. Also a debug log.\n"
            <> (BSL.unpack $ encodePretty st) <> "\n"
            <> (BSL.unpack $ encodePretty $ st ^. stSnek) <> "\n"
            <> (BSL.unpack $ encode $ st ^. stObjective) <> "\n"

bordersVertical :: Int
bordersVertical = 5

bordersHorizontal :: Int
bordersHorizontal = 2

drawWithShinyBorder :: GameState -> [Widget ()]
drawWithShinyBorder st =
  let score = show $ st ^. stObjective . oPoints
      time = show $ st ^. stCounter
   in [ joinBorders $
          withBorderStyle unicode $
            borderWithLabel (str "Snek") $
              vBox $
                drawBoard st
                  ++ [ hBorder,
                       vLimit 2 $
                         hLimitPercent 69 $
                           vBox
                             [ str "WASD to move" <+> fill ' ',
                               str "(Q) or (Esc) to quit. (P) to pause."
                             ]
                             <+> vBorder
                             <+> vBox
                               [ str $ "Score: " <> score,
                                 str $ "Time: " <> time
                               ]
                     ]
      ]

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
    _ -> return ()

  status <- use stStatus
  case status of
    Pause -> return ()
    Ended -> return ()
    Running -> case e of
      AppEvent Counter -> do
        stCounter %= (+ 1)
        gameEvent e
      _ -> gameEvent e

theApp :: App GameState CustomEvent ()
theApp =
  App
    { appDraw = drawWithShinyBorder,
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
