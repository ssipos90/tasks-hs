{-# LANGUAGE LambdaCase #-}
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
import Data.Time.Clock.POSIX (getPOSIXTime)
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

data AppPage e = CreatePage (CreateForm e)
               | ListPage


data AppState e = AppState
  { tasks :: L.List () Task,
    page :: AppPage e,
    messages :: [[Char]],
    filepath :: [Char]
  }

makeLensesFor [("messages", "_messages"), ("filepath", "_filepath"), ("tasks", "_tasks"), ("page", "_page")] ''AppState

mkCreateForm :: Task -> CreateForm e
mkCreateForm =
  F.newForm
    [ label "Work" @@= F.editTextField _title () (Just 1)
    ]
  where
    label s w =
      padBottom (T.Pad 1) $
        vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w

taskToItem :: Task -> T.Widget n
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
    ListPage -> [listUI s]
    CreatePage f -> [F.renderForm f]

listUI :: AppState e -> T.Widget ()
listUI s = vBox [ L.renderList (\_ task -> taskToItem task) True (s ^. _tasks)
                , str $ fromMaybe "" $ head (s ^. _messages)
                ]

completedTask :: A.AttrName
completedTask = A.attrName "completed"

attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, U.fg V.blue),
      (completedTask, U.fg V.green)
    ]

initialTask :: Task
initialTask =
  Task
    { title = "",
      addedAt = 1,
      finishedAt = Nothing
    }

listPageEvent :: AppState e1 -> T.BrickEvent n e2 -> T.EventM () (T.Next (AppState e1))
listPageEvent s (T.VtyEvent e) = case e of
  (V.EvKey V.KEsc []) -> M.halt s
  V.EvKey (V.KChar 'c') [] ->
    M.continue $ over _page (const $ CreatePage $ mkCreateForm initialTask) s
  V.EvKey (V.KChar 'w') [] -> do
    liftIO $ saveTasks (s ^. _filepath) (toList $ s ^. _tasks)
    M.continue $ over _messages ((:) "Written") s
  V.EvKey (V.KChar ' ') [] -> do
    ts <- round <$> (liftIO $ getPOSIXTime)
    M.continue $ over _tasks (L.listModify (toggleTask ts)) s
  _ -> do
    tasks <- L.handleListEventVi L.handleListEvent e (s ^. _tasks)
    M.continue $ over _tasks (const tasks) s
listPageEvent s _ = M.continue s

createPageHandler :: AppState e -> CreateForm e -> T.BrickEvent () e -> T.EventM () (T.Next (AppState e))
createPageHandler s f e = case e of
  T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s
  T.VtyEvent (V.EvKey V.KEnter []) -> do
    -- TODO validate
    ts <- round <$> liftIO getPOSIXTime
    let tasks = s ^. _tasks
    let newTasks = L.listInsert (Vec.length $ L.listElements tasks) (F.formState f){ addedAt = ts } tasks
    liftIO $ saveTasks (s ^. _filepath) (toList newTasks)
    M.continue $ s { tasks = newTasks
                   , page = ListPage
                   , messages = "Added and saved" : s ^. _messages
                   }
  _ -> do
    nf <- F.handleFormEvent e f
    M.continue $ over _page (\_ -> CreatePage nf) s

handleEvent :: AppState e -> T.BrickEvent () e -> T.EventM () (T.Next (AppState e))
handleEvent s e =
  case e of
    T.VtyEvent V.EvResize{} -> M.continue s
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

loadTasks :: FilePath -> IO (Either Error [Task])
loadTasks filepath = parseTasks <$> BS.readFile filepath

saveTasks :: FilePath -> [Task] -> IO ()
saveTasks fp tasks = BS.writeFile fp $ serializeTasks tasks

main :: IO ()
main = do
  args <- getArgs
  let filepath = fromMaybe defaultFile $ head args
  loadTasks filepath >>= \case
          Left e -> putStrLn e
          Right tasks -> void $ M.defaultMain app $
            AppState
              { tasks = L.list () (Vec.fromList tasks) 1,
                page = ListPage,
                filepath = filepath,
                messages = []
              }
