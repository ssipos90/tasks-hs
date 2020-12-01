{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import App
import qualified Brick.AttrMap as A
import Brick.Forms ((@@=))
import qualified Brick.Forms as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Util as U
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( fill,
    hLimit,
    padBottom,
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
import Lens.Micro (over, (^.))
import Lens.Micro.TH (makeLensesFor)
import Protolude

defaultFile :: FilePath
defaultFile = "tasks.json"

data TaskField
  = NameField
  | DescriptionField
  deriving (Eq, Ord, Show)

type CreateForm e = F.Form Task e ()

data AppPage e = CreatePage (CreateForm e) | ListPage | DetailPage

data AppState e = AppState
  { tasks :: L.List () Task,
    page :: AppPage e
  }

makeLensesFor [("tasks", "_tasks"), ("page", "_page")] ''AppState

mkCreateForm :: Task -> CreateForm e
mkCreateForm =
  F.newForm
    [ label "Work" @@= F.editTextField _title () (Just 1)
    ]
  where
    label s w =
      padBottom (T.Pad 1) $
        vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

taskToItem task =
  taskCheckMark
    <+> str (Tx.unpack $ task ^. _title)
  where
    taskCheckMark =
      if (isTaskDone task)
        then withAttr completedTask $ str " ✔ "
        else str "   "

ui :: AppState e -> [T.Widget ()]
ui s =
  case s ^. _page of
    ListPage -> [L.renderList (\_ task -> taskToItem task) True (s ^. _tasks)]
    CreatePage f -> [F.renderForm f]

completedTask = A.attrName "completed"

attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, U.fg V.blue),
      (completedTask, U.fg V.green)
    ]

initialTask =
  Task
    { title = "",
      addedAt = 0,
      finishedAt = Nothing
    }

listPageEvent s (T.VtyEvent e) = case e of
  (V.EvKey V.KEsc []) -> M.halt s
  V.EvKey (V.KChar 'c') [] ->
    M.continue $
      over
        _page
        (\_ -> CreatePage $ mkCreateForm initialTask)
        s
  e -> do
    tasks <- L.handleListEventVi L.handleListEvent e (s ^. _tasks)
    M.continue $ over _tasks (const tasks) s

createPageHandler :: AppState e -> CreateForm e -> T.BrickEvent () e -> T.EventM () (T.Next (AppState e))
createPageHandler s f e = case e of
  T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s
  T.VtyEvent (V.EvKey V.KEnter []) -> do
    let tasks = s ^. _tasks
    M.continue $ s { tasks = L.listInsert (Vec.length $ L.listElements tasks) (F.formState f) tasks
                   , page = ListPage
                   }
  _ -> do
    nf <- F.handleFormEvent e f
    M.continue $ over _page (\_ -> CreatePage nf) s

handleEvent :: AppState e -> T.BrickEvent () e -> T.EventM () (T.Next (AppState e))
handleEvent s e =
  case e of
    T.VtyEvent (V.EvResize {}) -> M.continue s
    _ -> case s ^. _page of
      CreatePage f -> createPageHandler s f e
      ListPage -> listPageEvent s e

app :: M.App (AppState e) e ()
app =
  M.App
    { M.appDraw = ui,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return,
      M.appAttrMap = const attributeMap
    }

main :: IO ()
main = do
  args <- getArgs
  let filename = fromMaybe defaultFile $ head args
  content <- BS.readFile filename
  case parseTasks content of
    Left error -> putStrLn $ "Error: " <> error
    Right tasks -> do
      let initialState =
            AppState
              { tasks = L.list () (Vec.fromList tasks) 1,
                page = ListPage
              }
      void $ M.defaultMain app initialState
