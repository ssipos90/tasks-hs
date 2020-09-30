{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified App
import Lens.Micro
import Lens.Micro.TH
import Brick.AttrMap (AttrMap)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (BrickEvent, CursorLocation, EventM, Next, Widget)
import qualified Brick.Types as T
import qualified Brick.Util as U
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    vBox,
    vLimit,
    withAttr,
    (<+>),
  )
import qualified Brick.Widgets.List as L
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Tx
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Protolude
import qualified System.IO as IO

defaultFile :: FilePath
defaultFile = "tasks.json"

data AppPage = CreatePage | ListPage

data AppState = AppState
  { _tasks :: L.List () App.Task,
    _page :: AppPage
  }

makeLenses ''AppState

taskTitle :: App.Task -> Tx.Text
taskTitle App.Task{ App.title } = title

taskToItem :: App.Task -> Tx.Text
taskToItem task =
  (if App.isTaskDone task then " x " else "   ")
    <> taskTitle task

ui :: AppState -> [Widget ()]
ui AppState {_tasks, _page} =
  case _page of
    ListPage -> [L.renderList (\_ -> str . Tx.unpack . taskToItem) True _tasks]
    CreatePage -> [C.center (str "Top") <+> B.hBorder <+> C.center (str "Bottom")]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ -- (L.listAttr, V.white `U.on` V.blue),
      (L.listSelectedAttr, V.blue `U.on` V.black),
      (customAttr, U.fg V.cyan)
    ]

appEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
appEvent s (T.VtyEvent e) =
  let p = s ^. page
  in
    case e of
      V.EvKey (V.KChar 'c') [] -> M.continue $ s {_page = CreatePage}
      V.EvKey (V.KChar 'k') [] -> M.continue $ s {_page = ListPage}
      V.EvKey (V.KChar 'j') [] -> M.continue $ s {_page = ListPage}
      V.EvKey (V.KChar _) [] -> M.continue s
      V.EvKey V.KEsc [] -> M.halt s
      e -> M.continue =<< L.handleListEventVi e (s ^. tasks)

app :: M.App AppState e ()
app =
  M.App
    { M.appDraw = ui,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  args <- getArgs
  let filename = fromMaybe defaultFile $ head args
  content <- BS.readFile filename
  case App.parseTasks content of
    Left error -> putStrLn $ "Error: " <> error
    Right tasks -> do
      let initialState =
            AppState
              { tasks = L.list () (Vec.fromList tasks) 1,
                page = ListPage
              }
      void $ M.defaultMain app initialState
