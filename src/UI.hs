{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}

module UI (drawUI, bordersHorizontal, bordersVertical) where

-- import Control.Lens ((~.))

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Lens.Micro ((^.))
import State
import UI.End (drawEndScreen)
import UI.Pause (drawPauseScreen)
import UI.Running (drawRunningScreen)

drawScreen :: GameState -> [Widget ()]
drawScreen st = case st ^. stStatus of
  Running -> drawRunningScreen st
  Ended -> drawEndScreen st
  Pause -> drawPauseScreen st
  _ -> []

bordersVertical :: Int
bordersVertical = 5

bordersHorizontal :: Int
bordersHorizontal = 2

drawUI :: GameState -> [Widget ()]
drawUI st = case st ^. stStatus of
  Menu -> []
  _ ->
    [ joinBorders $
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
    where
      score = show $ st ^. stObjective . oPoints
      time = show $ st ^. stCounter
