module UI.Pause (drawPauseScreen) where

import Brick (Widget, fill, strWrapWith)
import Brick.Widgets.Core (str)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (encode)
import Lens.Micro ((^.))
import State
import Text.Wrap (defaultWrapSettings, preserveIndentation)
import Data.ByteString.Lazy.Char8 as BSL

drawPauseScreen :: GameState -> [Widget ()]
drawPauseScreen st = [ str "Press (P) to unpause. Also a debug log.\n",
      strWrapping (BSL.unpack $ encodePretty st),
      strWrapping (BSL.unpack $ encodePretty $ st ^. stSnek),
      strWrapping (BSL.unpack $ encode $ st ^. stObjective), -- TODO(rado) this doesn't wrap at all
      fill ' '
    ]
    where
      settings = defaultWrapSettings {preserveIndentation = True}
      strWrapping = strWrapWith settings