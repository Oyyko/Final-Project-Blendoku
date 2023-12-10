module InputPlayerName
  ( inputPlayerName,
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, editor, getEditContents, handleEditorEvent, renderEditor)
import Graphics.Vty

app :: App (Editor String ()) e ()
app =
  App
    { appDraw = ui,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const $ attrMap defAttr [],
      appChooseCursor = neverShowCursor
    }

ui :: (Editor String ()) -> [Widget ()]
ui ed =
  [ padLeft (Pad 19) $
      padRight (Pad 21) $
        center $
          vLimit 15 $
            hLimit 30 $
              withBorderStyle unicodeBold $
                borderWithLabel (str "Blendoku") $
                  center $
                  vBox [
                      str "Type your name:\n",
                      borderWithLabel (str "Player Name") $ vBox $
                      [
                        renderEditor (str . unlines) True ed
                      ],
                      str "Press 'Enter' to continue"
                    ]
  ]

handleEvent :: BrickEvent () e -> EventM () (Editor String ()) ()
handleEvent (VtyEvent (EvKey KEsc _)) = halt
handleEvent (VtyEvent (EvKey (KChar 'q') _)) = halt
handleEvent (VtyEvent (EvKey (KChar 'Q') _)) = halt
handleEvent (VtyEvent (EvKey (KEnter) _)) = halt
handleEvent ev = handleEditorEvent ev

inputPlayerName :: IO [String]
inputPlayerName = defaultMain app (editor () (Just 1) "") >>= \x -> return $ getEditContents x