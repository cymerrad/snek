{-# LANGUAGE LambdaCase #-}

module UI.Menu (getPlayerCount) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types
  ( BrickEvent (..),
    Widget,
  )
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( padAll,
    str,
  )
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V

data Choice = Red | Blue | Green
  deriving (Show)

data Name
  = RedButton
  | BlueButton
  | GreenButton
  deriving (Show, Eq, Ord)

drawMenu :: D.Dialog Choice Name -> [Widget Name]
drawMenu d = [ui]
  where
    ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "A game for how many, sir or ma'am?"

appEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> M.halt
    _ -> D.handleDialogEvent ev
appEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str "Snek") (Just (BlueButton, choices)) 50
  where
    choices =
      [ ("0 Players", RedButton, Red),
        ("1 Player", BlueButton, Blue),
        ("2 Players", GreenButton, Green)
      ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
    ]

theMenu :: M.App (D.Dialog Choice Name) e Name
theMenu =
  M.App
    { M.appDraw = drawMenu,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

runMenu :: IO (D.Dialog Choice Name)
runMenu = M.defaultMain theMenu initialState

getGame :: IO (Maybe (Name, Choice))
getGame = D.dialogSelection <$> runMenu

getPlayerCount :: IO Int
getPlayerCount =
  getGame
    >>= ( \case
            Nothing -> return 0
            something -> case something of
              Just (_, Red) -> return 0
              Just (_, Blue) -> return 1
              Just (_, Green) -> return 2
        )
