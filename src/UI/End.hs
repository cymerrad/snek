module UI.End (drawEndScreen) where
import State (GameState)
import Brick (Widget)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (str)

drawEndScreen :: GameState -> [Widget ()]
drawEndScreen _ = [center . str $ "Game over"]