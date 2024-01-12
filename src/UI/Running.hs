module UI.Running (drawRunningScreen) where

import Brick
import Lens.Micro ((^.))
import State

drawMatrix :: CharMatrix -> [Widget ()]
drawMatrix = map str

drawRunningScreen :: GameState -> [Widget ()]
drawRunningScreen st = [a]
  where
    grid = st ^. stGrid
    a = foldl1 (<=>) (drawMatrix grid)