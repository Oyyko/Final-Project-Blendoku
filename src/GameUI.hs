{-# LANGUAGE TemplateHaskell #-}

module GameUI
    (
        playGame
    ) where

import Control.Lens (makeLenses, (^.), (<&>))
import qualified Brick.Widgets.Center as C
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Data.Map as M (Map, filterWithKey, fromList, toList, elems)

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

-- drawUI :: UI -> [Widget Name]
-- drawUI ui =
--   [ 
--     C.center 
--   $ vBox 
--     [
--         drawBoard ui
--     ]
--   ]


drawUI :: UI -> [Widget Name]
drawUI ui =
    [
        vBox $ replicate 2 (hBox (     
                replicate 10 (padLeft (Pad 1) $ drawRectangleWithColor)    
        )) 
    ]

drawRectangleWithColor :: Widget n
drawRectangleWithColor =
  B.border $
  vBox [ str "     "
       , str "     "
       ]

drawBoard :: UI -> Widget Name
drawBoard ui =
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Blendoku")
    $ if ui ^. paused then
        C.center $ str "Paused"
    else
      vBox $ [boardHeight, boardHeight - 1 .. 1] <&> \r ->
          foldr (<+>) emptyWidget
            . M.filterWithKey (\(V2 _ y) _ -> r == y)
            $ mconcat
                [
                    drawBoardPlay (ui ^. game . board)
                ,   emptyCellMap
                ]


drawBoardPlay :: Board -> Map Coord (Widget Name)
drawBoardPlay board = M.fromList
   (map cellToInfo (M.toList board)) where
        cellToInfo :: (Cell, Coord) -> (Coord, Widget Name)
        cellToInfo (cell, coord) = (coord, cellToWidget cell)

cellToWidget :: Cell -> Widget Name
cellToWidget cell = withAttr (cellToAttr cell) cw
    where cellToAttr cell = attrName (colorToNameGray (cell ^. color))

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
gameAttrMap = attrMap V.defAttr
    [(attrName (colorToNameGray val), bg (V.RGBColor (fromIntegral val) (fromIntegral val) (fromIntegral val))) | val <- [0..255]]

testColor1 ::V.Color
testColor1 = V.RGBColor 255 0 0

testColor2 ::V.Color
testColor2 = V.RGBColor 0 255 0

emptyCellMap :: Map Coord (Widget Name)
emptyCellMap = M.fromList
  [ (V2 x y, emptyGridCellW) | x <- [1 .. boardWidth], y <- [1 .. boardHeight] ]

emptyGridCellW :: Widget Name
emptyGridCellW = withAttr emptyAttr cw

emptyAttr :: AttrName
emptyAttr = attrName "empty"

cw :: Widget Name
cw = str "  "

boardWidth :: Int
boardHeight ::Int
cellSize :: Int
boardWidth = 10
boardHeight = 10
cellSize = 1

