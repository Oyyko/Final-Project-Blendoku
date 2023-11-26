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

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 15
    $ hLimit 30
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Blendoku")
    $ C.center
    $ vBox
        [ str "Game Type (0-2)"
        , str "  0: Line"
        , str "  1: Rectangle"
        , str "  2: TShape"
        , str "  3: HShape"
        ]

handleEvent :: BrickEvent () e -> EventM () (Maybe Int) ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar d) [])) =
  when (d `elem` ['0' .. '3']) $ do
    put $ Just $ read [d]
    halt
handleEvent _ = pure ()

pickGameType :: IO Int
pickGameType = defaultMain app Nothing >>= maybe exitSuccess return
