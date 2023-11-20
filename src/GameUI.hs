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

drawUI :: UI -> [Widget n]
drawUI ui =
    [
        drawCandidates ui candidateRows candidateCols
       , drawCandidates ui candidateRows candidateCols
    ]

-- drawPlaygrounds :: UI -> Int -> Int -> Widget n
-- drawPlaygrounds ui rows cols = 
--     C.center $
--     B.borderWithLabel (str "Playground") $
--     vBox $ [1 .. rows] <&> \r ->
--         foldr (<+>) emptyWidget
--             . M.filterWithKey (\(V2 _ y) _ -> r == y)
--             $ mconcat
--                 [
--                     drawBoardPlay (ui ^. game . board)
--                    ,emptyWidgetMap rows cols
--                 ]


drawCandidates :: UI -> Int -> Int -> Widget n
drawCandidates ui rows cols = 
    C.center $
    B.borderWithLabel (str "Candidates") $
    vBox $ [1 .. rows] <&> \r ->
        foldr (<+>) emptyWidget
            . M.filterWithKey (\(V2 _ y) _ -> r == y)
            $ mconcat
                [
                    drawBoardPlay (ui ^. game . board)
                   ,emptyWidgetMap rows cols
                ]

drawBoardPlay :: Board -> Map Coord (Widget n)
drawBoardPlay board = M.fromList
   (map cellToInfo (M.toList board)) where
        cellToInfo :: (Cell, Coord) -> (Coord, Widget n)
        cellToInfo (cell, coord) = (coord, cellToWidget cell)

cellToWidget :: Cell -> Widget n
cellToWidget cell = if cell ^. chosen 
    then padLeft (Pad 1) $ B.border $ drawRectangleWithColor (cell ^. color)
    else padTopBottom 1 $ padLeft (Pad 1) $  drawRectangleWithColor (cell ^. color)

emptyWidgetMap :: Int -> Int -> Map Coord (Widget n)
emptyWidgetMap rows cols = M.fromList
  [ (V2 c r, emptyGridW) | r <- [1 .. rows], c <- [1 .. cols] ]

emptyGridW :: Widget n
emptyGridW = padLeft (Pad 1) $ drawRectangleWithColor 255

drawRectangleWithColor :: Int -> Widget n
drawRectangleWithColor val =
  vBox 
  [
    withAttr (attrName ("gray " ++ show val))  (str "     ")
   , withAttr (attrName ("gray " ++ show val))  (str "     ")
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
gameAttrMap = attrMap V.defAttr
    [(attrName (colorToNameGray val), bg (V.RGBColor (fromIntegral val) (fromIntegral val) (fromIntegral val))) | val <- [0..255]]

candidateRows, candidateCols :: Int
candidateRows = 1
candidateCols = 10

