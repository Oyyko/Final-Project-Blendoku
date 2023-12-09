module PickGameType
  ( pickGameType
  ) where

import System.Exit (exitSuccess)
import Control.Monad (when)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

app :: App ((Maybe Int),String) e ()
app = App
  { appDraw         = ui
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: ((Maybe Int),String) -> [Widget ()]
ui st =
  [padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 15
    $ hLimit 30
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str ("Blendoku For: " ++ (snd st)))
    $ C.center
    $ vBox
        [ str "Main Menu: Game Type (0-5)"
        , str "  0: Line"
        , str "  1: Rectangle"
        , str "  2: TShape"
        , str "  3: HShape"
        , str "  4: Random" 
        , str "  5: Challenge" --challenge mode
        ]
  ]

handleEvent :: BrickEvent () e -> EventM () ((Maybe Int),String) ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar d) [])) =
  when (d `elem` ['0' .. '5']) $ do
    st <- get
    put $ (Just (read [d]), (snd st))
    halt
handleEvent _ = pure ()

pickGameType :: String -> IO Int
pickGameType playerName = defaultMain app (Nothing,playerName) >>= \x -> maybe exitSuccess return (fst x)
