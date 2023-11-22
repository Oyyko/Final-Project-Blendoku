{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module GameUI
    (
        playGame
    ) where

import Control.Lens hiding (preview, op, zoom, (<|), chosen)
import qualified Brick.Widgets.Center as C
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever, when)
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Data.Map as M (Map, filterWithKey, fromList, toList, elems, size)
import Data.List (sort)

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

drawUI ui =
  [ C.center
  $ vBox
      [
        drawCandidates ui candidateRows candidateCols
      --  ,drawHelp
        ,hBox
          [
            drawGameState (ui ^. game)
          , drawHelp
          ]
      ]
  ]

drawCandidates :: UI -> Int -> Int -> Widget n
drawCandidates ui rows cols =
    B.borderWithLabel (str "Candidates") $
    vBox $ [1 .. rows] <&> \r ->
        foldr (<+>) emptyWidget
            . M.filterWithKey (\(V2 _ y) _ -> r == y)
            $ mconcat
                [
                    drawBoardPlay (ui ^. game . board)
                   ,emptyWidgetMap rows cols
                ]

drawHelp :: Widget Name
drawHelp =
     B.borderWithLabel (str "Help")
    $ hLimit 20
    $ vLimit 20
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("Left"   , "h, ←")
      , ("Right"  , "l, →")
      , ("Choose",  "Space")
      , ("Swap",   "Enter")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

drawBoardPlay :: Board -> Map Coord (Widget n)
drawBoardPlay board = M.fromList
   (map cellToInfo (M.toList board)) where
        cellToInfo :: (Coord, Cell) -> (Coord, Widget n)
        cellToInfo (coord, cell) = (coord, cellToWidget cell)

drawGameState :: Game -> Widget Name
drawGameState g = 
    B.borderWithLabel (str "Game State")
    $ padTopBottom 1
    $ vBox
      [ 
        padLeftRight 4 $ 
        if isGameEnd g then str "Game End"
        else str "Correct: " <+> str (show (countEqual g))
      ]

cellToWidget :: Cell -> Widget n
cellToWidget cell
  | cell ^. chosen = padLeft (Pad 1) $ withBorderStyle BS.unicodeBold $ B.border $ drawRectangleWithColor (cell ^. color)
  | cell ^. hovered = padLeft (Pad 1) $ B.border $ drawRectangleWithColor (cell ^. color)
  | otherwise = padTopBottom 1 $ padLeft (Pad 1) $ drawRectangleWithColor (cell ^. color)

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
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = exec (toggleSelection)
handleEvent (VtyEvent (V.EvKey V.KEnter      [])) = exec (swapWithChosen)
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = exec (shift R)
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift L)
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = exec (shift D)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = exec (shift U)
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent _ = pure ()

gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
    [(attrName (colorToNameGray val), bg (V.RGBColor (fromIntegral val) (fromIntegral val) (fromIntegral val))) | val <- [0..255]]


exec :: BlendokuGame () -> EventM Name UI ()
exec op =
  guarded
    (not . \ui -> ui ^. paused && isGameEnd (ui ^. game))
    (game %~ execBlendokuGame op)


guarded :: (UI -> Bool) -> (UI -> UI) -> EventM Name UI ()
guarded p f = do
  ui <- get
  when (p ui && not (ui ^. game . to isGameEnd)) $
    modify f

-- check whether the two boards are the same state
-- to be more accurate, check whether the cell in the same position has the same color
-- ignore cursor/chosen


withSameState :: Board -> Board -> Bool
withSameState board gtBoard =
  let items = map (\(k, Cell c _ _) -> (k, c)) (M.toList board)
      itemsGt = map (\(k, Cell c _ _) -> (k, c)) (M.toList gtBoard)
  in sort items == sort itemsGt

isGameEnd :: Game -> Bool
isGameEnd g = (countEqual g) == g ^.board . to M.size

countEqual :: Game -> Int
countEqual g = length $ filter (uncurry (==)) (zip xs ys)
  where 
    xs = f (g ^.board )
    ys = f (g ^. gtBoard)
    f b = sort (map (\(k, Cell c _ _) -> (k, c)) (M.toList b))

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"