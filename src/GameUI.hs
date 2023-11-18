{-# LANGUAGE TemplateHaskell #-}

module GameUI 
    (
        playGame
    ) where

import Control.Lens (makeLenses, (^.))
import qualified Brick.Widgets.Center as C
import Brick 
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import qualified Graphics.Vty as V

import Blendoku

data Tick = Tick
type Name = ()

data UI = UI
  { 
    _game    :: Game         -- ^ blendoku game
  , _paused  :: Bool         -- ^ game paused
  }

makeLenses ''UI

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const gameAttrMap
  }

drawUI :: UI -> [Widget Name]
drawUI ui =
  [ C.center 
  $ vLimit 22 
  $ padLeft (Pad 25)
  $ padRight (Pad 21)
  $ hBox
      [ 
        cellWidget
      ]
  ]

playGame :: IO Game
playGame = do
  let delay = 100000
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  ui <- customMain initialVty builder (Just chan) app $ UI
    { _game    = initialGame
    , _paused  = False
    }
  return $ ui ^. game

handleEvent :: BrickEvent Name Tick -> EventM Name UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent _ = pure ()

gameAttrMap :: AttrMap
gameAttrMap = attrMap
  V.defAttr
  [ 
    (cellAttr, testColor1 `on` testColor2)
  ]

cellWidget :: Widget Name
cellWidget = withAttr cellAttr $ str "Hello World"

cellAttr :: AttrName
cellAttr = attrName "cell"

testColor1 ::V.Color
testColor1 = V.RGBColor 255 0 0

testColor2 ::V.Color
testColor2 = V.RGBColor 0 255 0

