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
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Data.Map as M (Map, filterWithKey, fromList, toList, elems, size, (!))
import Data.List (sort)

import Blendoku

data Tick = Tick
type Name = ()

data UI = UI    --不需要用UI的参数
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
  hLimit 50 $
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
                    drawBoardPlay g (g ^. board) endOfGame
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
      , ("Choose",  "Space")
      , ("Swap",   "Enter")
      , ("Quit"   , "q")
      , ("RGB/Gray", "g")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

drawBoardPlay :: Game -> Board -> Bool -> Map Coord (Widget n)
drawBoardPlay g board endOfGame = M.fromList
   (map cellToInfo (M.toList board)) where
        cellToInfo :: (Coord, Cell) -> (Coord, Widget n)
        cellToInfo (coord, cell) = (coord, cellToWidget g cell endOfGame)

drawGameState :: Game -> Widget Name
drawGameState g =
    let title = if g ^.isChallenge then "Challenge Mode" else "Normal Mode" in
    B.borderWithLabel (str title)
    $ hLimit 25
    $ padTopBottom 1
    $ vBox
      [
         drawPointerState g
        , if g ^.hint then drawHint g else emptyWidget
        , if g ^.isChallenge then 
          vBox
          [
             str "  Level: " <+> str (show (g ^. level)) <+> str "/" <+> str (show (g ^. maxLevel))
            , if isGameEnd g 
              then 
                if g ^. level == g ^. maxLevel 
                  then vBox
                  [
                    str "  Congratulations!",
                    str "  Blendoku Master!",
                    str "  Exit in " <+> str (show (getRemainTimeInSec g)) <+> str "s"
                  ]
                  else vBox
                  [
                    str "  Level Completed!",
                    str "  Next level in " <+> str (show (getRemainTimeInSec g)) <+> str "s" 
                  ]
              else vBox
                [
                  str "  Remaining Time: " <+> str (show (getRemainTimeInSec g)) <+> str "s",
                  if g ^. remainTime <=30 
                    then vBox 
                    [
                      str "  Time's nearly up!",
                      str "Challenge will fail if not completed in time!" 
                    ]
                    else emptyWidget
                ]
          ]
          else if isGameEnd g 
            then vBox
              [
                str "  Game Completed!",
                str "  Exit in " <+> str (show (getRemainTimeInSec g)) <+> str "s"
              ]
            else emptyWidget
      ]


getRemainTimeInSec :: Game -> Int
getRemainTimeInSec g = g ^. remainTime `div` timeScale

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
    ]
    where cell = (g ^. board) M.! (g ^. cursorPos)

drawCursorGrid :: Game -> Widget n
drawCursorGrid g =
  padLeftRight 2 $
  vBox
  [
      str (show (g ^. cursorPos))
    , drawRectangleWithColor isGrayMode (cell ^. color)
    , str "Cursor"
    , if cell ^. locked then str "locked" else str "free"
  ]
  where cell = (g ^. board) M.! (g ^. cursorPos)
        isGrayMode = g ^. isGray

drawChosenGrid :: Game -> Widget n
drawChosenGrid g =
  padLeftRight 2 $
  if (g ^. chosenPos) == V2 0 0
      then vBox
      [
        str "Empty"
      , emptyGridW
      , str "Chosen"
      ]
      else vBox
      [
        str (show (g ^. chosenPos))
      , drawRectangleWithColor isGrayMode (cell ^. color)
      , str "Chosen"
      ]
      where cell = (g ^. board) M.! (g ^. chosenPos)
            isGrayMode = g ^. isGray


drawHint :: Game -> Widget n
drawHint g =
  B.borderWithLabel (str "Hint") $
  vBox
  [
      str "correct / changeable"
    , str "Progress: " <+> str (show (countCorrectChangeable g)) <+> str "/" <+> str (show (countChangeable g))
  ]

cellToWidget :: Game -> Cell -> Bool -> Widget n
cellToWidget g cell endOfGame
  | endOfGame = drawRectangleWithColor isGrayMode (cell ^. color)
  | cell ^. chosen = drawRectangleWithAttr isGrayMode (cell ^. color) "chosen"
  | cell ^. hovered = drawRectangleWithAttr isGrayMode (cell ^. color) "hover"
  | otherwise = drawRectangleWithColor isGrayMode (cell ^. color)
  where isGrayMode = g ^. isGray

emptyWidgetMap :: Int -> Int -> Map Coord (Widget n)
emptyWidgetMap rows cols = M.fromList
  [ (V2 c r, emptyGridW) | r <- [1 .. rows], c <- [1 .. cols] ]

emptyGridW :: Widget n
emptyGridW = padLeft (Pad 1) $ drawRectangleWithColor True (254, 254, 254)

drawRectangleWithAttr :: Bool -> ColorVector ->String -> Widget n
drawRectangleWithAttr grayMode color name =
  let char = if name == "chosen" then "◼︎" else "◻︎" in
    vBox
  [
      withAttr (attrName (colorToNameRGB c')) (str (" " ++ char ++ " " ++ char ++ " "))
    ,  withAttr (attrName (colorToNameRGB c')) (str (" " ++ char ++ " " ++ char ++ " "))
       ]
    where c' = if grayMode then bgrToGray color else color

drawRectangleWithColor :: Bool -> ColorVector -> Widget n
drawRectangleWithColor grayMode color =
    vBox
  [
      withAttr (attrName (colorToNameRGB c')) (str "     ")
   ,  withAttr (attrName (colorToNameRGB c')) (str "     ")
  ]
  where c' = if grayMode then bgrToGray color else color

bgrToGray :: ColorVector -> ColorVector
bgrToGray (r, g, b) = (v', v', v')
  where 
    v' = (round v) `div` 4 * 4
    v = 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b

-- Main Func
playGame :: Int -> IO Game
playGame gameType = do
  let delay = 1000           -- unit: microsecond (1 second = 1,000,000 microseconds)
  chan <- newBChan 10
  void . forkIO $ forever $ do      -- 无限循环：Tick计时
    writeBChan chan Tick
    threadDelay delay
  (lvl, isChallenge) <- if gameType == 5 then return (0, True) else return (gameType, False)
  initialGame <- initGame isChallenge lvl
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
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = exec (toggleGray)
-- handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = exec (toggleLock)
-- handleEvent (VtyEvent (V.EvKey (V.KChar 'n') [])) = goToNextLevel
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = exec (shift R)
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift L)
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = exec (shift D)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = exec (shift U)
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (AppEvent Tick) = do
  g <- use game
  if isGameEnd g then do
    x <- use $ game . remainTime
    if x > 10 * timeScale then do
      game . remainTime .= 10 * timeScale
    else
      if x > 0 then do
        game . remainTime -= 1
      else
        if g ^.isChallenge && g ^. level < g ^. maxLevel 
          then goToNextLevel
          else halt
  else do
    exec timeTickPerGame
handleEvent _ = pure ()

--  support RGB/gray color
-- for gray color, the color is from 0 to 255
-- for RGB color, the color value is set to even to reduce the time to generate the color map
-- Currently it takes about 5 seconds to generate the color map
gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
    ([(attrName (colorToNameGray val), bg (V.RGBColor (fromIntegral val) (fromIntegral val) (fromIntegral val))) | val <- [0..255]]
    ++ [(attrName (colorToNameRGB (r, g, b)), bg (V.RGBColor (fromIntegral r) (fromIntegral g) (fromIntegral b))) | r <- [0, 4..255], g <- [0, 4..255], b <- [0, 4..255]]
    )

goToNextLevel :: EventM Name UI ()
goToNextLevel = do
  lvl <- use $ game . level
  g <- liftIO $ initGame True (lvl + 1)
  game .= g

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
isGameEnd g = (countEqual g) == M.size (g ^.board)

countLocked :: Game -> Int
countLocked g = length $ filter (\(_, Cell _ _ _ l) -> l) (M.toList (g ^. board))

countBlack :: Game -> Int
countBlack g = length $ filter (\(_, Cell c _ _ _) -> c == (0, 0, 0)) (M.toList (g ^. board))

countChangeable :: Game -> Int
countChangeable g = M.size (g ^.board) - countLocked g - countBlack g

countCorrectChangeable :: Game -> Int
countCorrectChangeable g = countEqual g - countLocked g - countBlack g

countEqual :: Game -> Int
countEqual g = length $ filter (uncurry (==)) (zip xs ys)
  where
    xs = f (g ^.board )
    ys = f (g ^. gtBoard)
    f b = sort (map (\(k, Cell c _ _ _) -> (k, c)) (M.toList b))

timeScale :: Int
timeScale = 1000



