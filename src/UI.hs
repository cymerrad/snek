{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
module UI (loop) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
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
import Graphics.Vty qualified as V

data CustomEvent = Counter deriving (Show)

data St = St
  { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent),
    _stCounter :: Int
  }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [a]
  where
    a =
      (str $ "Last event: " <> (show $ st ^. stLastBrickEvent))
        <=> (str $ "Counter value is: " <> (show $ st ^. stCounter))

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e =
  case e of
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent _ -> stLastBrickEvent .= (Just e)
    AppEvent Counter -> do
      stCounter %= (+ 1)
      stLastBrickEvent .= (Just e)
    _ -> return ()

initialState :: St
initialState =
  St
    { _stLastBrickEvent = Nothing,
      _stCounter = 0
    }

theApp :: App St CustomEvent ()
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
  void $ customMain initialVty buildVty (Just chan) theApp initialState
