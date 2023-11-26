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
import Data.Map as M (Map, filterWithKey, fromList, toList, elems, size, (!))
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
        padLeftRight 4 $ drawCandidates (ui ^. game) (ui ^. (game . boardRows) ) (ui ^. (game . boardCols) )
        , padLeftRight 4 $ drawInfo (ui ^. game)
      ]
  ]

drawInfo :: Game -> Widget Name
drawInfo g =
  
  hBox
  [
    drawGameState g
    , drawHelp
  ]

drawCandidates :: Game -> Int -> Int -> Widget n
drawCandidates g rows cols =
    B.borderWithLabel (str "Candidates") $
    vBox $ [1 .. rows] <&> \r ->
        foldr (<+>) emptyWidget
            . M.filterWithKey (\(V2 _ y) _ -> r == y)
            $ mconcat
                [
                    drawBoardPlay (g ^. board) endOfGame
                   ,emptyWidgetMap rows cols
                ]
    where endOfGame = isGameEnd g

drawHelp :: Widget Name
drawHelp =
     B.borderWithLabel (str "Help")
    $ hLimit 20
    $ vLimit 20
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("Left"   , "←")
      , ("Right"  , "→")
      , ("Up"     , "↑")
      , ("Down"   , "↓")
      , ("Hint"   , "h")
      , ("Lock"   , "l")
      , ("Choose",  "Space")
      , ("Swap",   "Enter")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

drawBoardPlay :: Board -> Bool -> Map Coord (Widget n)
drawBoardPlay board endOfGame = M.fromList
   (map cellToInfo (M.toList board)) where
        cellToInfo :: (Coord, Cell) -> (Coord, Widget n)
        cellToInfo (coord, cell) = (coord, cellToWidget cell endOfGame)

drawGameState :: Game -> Widget Name
drawGameState g =
    B.borderWithLabel (str "Game State")
    $ padTopBottom 1
    $ vBox
      [
          padLeftRight 4 $ if isGameEnd g then str "Task Completed" else str "Task In Progress"
        , drawPointerState g
        , padLeftRight 4 $ str ("#correct = " ++  if g ^.hint then show (countEqual g) else "??")
      ]

drawPointerState :: Game -> Widget n
drawPointerState g =
    padLeftRight 1
    $ vBox
    [
        hBox
        [
            drawCursorGrid g
          , drawChosenGrid g
        ]
      , padLeftRight 4 $ str "Locked: " <+> str (show (cell ^. locked))
    ]
    where cell = (g ^. board) M.! (g ^. cursorPos)

drawCursorGrid :: Game -> Widget n
drawCursorGrid g = B.border $
  vBox
  [
      str "Cursor: "
    , str (show (g ^. cursorPos))
    , padLeftRight 1 $ drawRectangleWithColor (cell ^. color)
  ]
  where cell = (g ^. board) M.! (g ^. cursorPos)

drawChosenGrid :: Game -> Widget n
drawChosenGrid g = B.border $
  if (g ^. chosenPos) == V2 0 0
      then vBox
      [
        str "Chosen: "
      , str "Empty"
      , emptyGridW
      ]
      else vBox
      [
        str "Chosen: "
      , str (show (g ^. chosenPos))
      , padLeftRight 1 $ drawRectangleWithColor (cell ^. color)
      ]
      where cell = (g ^. board) M.! (g ^. chosenPos)

cellToWidget :: Cell -> Bool -> Widget n
cellToWidget cell endOfGame
  | endOfGame = drawRectangleWithColor (cell ^. color)
  | cell ^. chosen = drawRectangleWithAttr "chosen"
  | cell ^. hovered = drawRectangleWithAttr "hover"
  | otherwise = drawRectangleWithColor (cell ^. color)

emptyWidgetMap :: Int -> Int -> Map Coord (Widget n)
emptyWidgetMap rows cols = M.fromList
  [ (V2 c r, emptyGridW) | r <- [1 .. rows], c <- [1 .. cols] ]

emptyGridW :: Widget n
emptyGridW = padLeft (Pad 1) $ drawRectangleWithColor (254, 254, 254)

drawRectangleWithAttr :: String -> Widget n
drawRectangleWithAttr name =
    vBox
  [
      withAttr (attrName name) (str "     ")
   ,  withAttr (attrName name) (str "     ")
       ]

drawRectangleWithColor :: ColorVector -> Widget n
drawRectangleWithColor color =
    vBox
  [
      withAttr (attrName (colorToNameRGB color)) (str "     ")
   ,  withAttr (attrName (colorToNameRGB color)) (str "     ")
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
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = exec (toggleHint)
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = exec (toggleLock)
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = exec (shift R)
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift L)
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = exec (shift D)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = exec (shift U)
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent _ = pure ()

--  support RGB/gray color
-- for gray color, the color is from 0 to 255
-- for RGB color, the color value is set to even to reduce the time to generate the color map
-- Currently it takes about 5 seconds to generate the color map
gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
    ([(attrName (colorToNameGray val), bg (V.RGBColor (fromIntegral val) (fromIntegral val) (fromIntegral val))) | val <- [0..255]]
    ++ [(attrName (colorToNameRGB (r, g, b)), bg (V.RGBColor (fromIntegral r) (fromIntegral g) (fromIntegral b))) | r <- [0, 4..255], g <- [0, 4..255], b <- [0, 4..255]]
    ++ [(attrName "chosen", bg V.blue), (attrName "hover", bg V.cyan), (attrName "locked", bg V.red)])

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
withSameState board1 board2 =
  let items1 = map (\(k, Cell c _ _ _) -> (k, c)) (M.toList board1)
      items2 = map (\(k, Cell c _ _ _) -> (k, c)) (M.toList board2)
  in sort items1 == sort items2

isGameEnd :: Game -> Bool
isGameEnd g = (countEqual g) == g ^.board . to M.size

countEqual :: Game -> Int
countEqual g = length $ filter (uncurry (==)) (zip xs ys)
  where
    xs = f (g ^.board )
    ys = f (g ^. gtBoard)
    f b = sort (map (\(k, Cell c _ _ _) -> (k, c)) (M.toList b))