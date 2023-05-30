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
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Graphics.Vty (Output (displayBounds), outputIface, regionHeight, regionWidth)
import Graphics.Vty qualified as V
import Logic (gameEvent)
import State
import Text.Wrap (defaultWrapSettings, preserveIndentation)

drawMatrix :: CharMatrix -> [Widget ()]
drawMatrix = map str

drawScreen :: GameState -> [Widget ()]
drawScreen st = case st ^. stStatus of
  Running -> [a]
    where
      grid = st ^. stGrid
      a = foldl1 (<=>) (drawMatrix grid)
  Ended -> [center . str $ "Game over"]
  Pause ->
    [ str "Press (P) to unpause. Also a debug log.\n",
      strWrapping (BSL.unpack $ encodePretty st),
      strWrapping (BSL.unpack $ encodePretty $ st ^. stSnek),
      strWrapping (BSL.unpack $ encode $ st ^. stObjective), -- TODO(rado) this doesn't wrap at all
      fill ' '
    ]
    where
      settings = defaultWrapSettings {preserveIndentation = True}
      strWrapping = strWrapWith settings
  Menu -> [] -- TODO(rado)

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
                drawScreen st
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
    { appDraw = drawWithShinyBorder,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr []
    }

-- TODO(rado) refactor
loop :: IO ()
loop = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan Counter
    threadDelay 1000000

  let buildVty = V.mkVty V.defaultConfig
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
