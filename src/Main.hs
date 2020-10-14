{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import Lens.Micro ( (^.), over )
import Lens.Micro.TH ( makeLensesFor )
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Forms as F
import Brick.Forms ((@@=))
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
    padBottom,
    fill,
    (<+>),
  )
import qualified Brick.Widgets.List as L
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Tx
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Protolude

defaultFile :: FilePath
defaultFile = "tasks.json"


data TaskField = NameField
               | DescriptionField
               deriving (Eq, Ord, Show)
type CreateForm = F.Form Task () ()
data AppPage = CreatePage CreateForm | ListPage | DetailPage

data AppState = AppState
  { tasks :: L.List () Task
  , page :: AppPage
  }

makeLensesFor [("tasks", "_tasks"), ("page", "_page")] ''AppState

mkCreateForm :: Task -> CreateForm
mkCreateForm = F.newForm [ label "Work" @@= F.editTextField _title () (Just 1) 
                         ]
               where label s w = padBottom (T.Pad 1) $
                        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w 

taskToItem task = (case isTaskDone task of
                    True -> withAttr completedTask $ str " ✔ "
                    False -> str "   "
                  )
              <+> (str $ Tx.unpack $ task ^. _title)

ui :: AppState -> [T.Widget ()]
ui s =
  case s ^. _page of
    ListPage -> [L.renderList (\_ task -> taskToItem task) True (s ^. _tasks)]
    CreatePage f -> [ F.renderForm f ]

completedTask = A.attrName "completed"

attributeMap :: A.AttrMap
attributeMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, U.fg V.blue)
    , (completedTask, U.fg V.green) 
    ]

createPageEvent :: AppState -> T.BrickEvent () e -> CreateForm -> T.EventM () (T.Next AppState)
createPageEvent s e f = case e of
      T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s
      e -> do
        let nf = F.handleFormEvent e f
        M.continue $ over _page (\_ -> nf) s

listPageEvent s (T.VtyEvent e) = case e of
      V.EvKey (V.KChar 'c') [] -> M.continue $ over _page (\_ -> 
                                    let initialTask = Task { title = ""
                                                            , addedAt = 0
                                                            , finishedAt = Nothing
                                                            }
                                    in CreatePage $ mkCreateForm initialTask
                             ) s
      V.EvKey (V.KChar 'j') [] -> M.continue $ over _tasks L.listMoveDown s
      V.EvKey (V.KChar 'k') [] -> M.continue $ over _tasks L.listMoveUp s
      V.EvKey (V.KChar 'g') [] -> M.continue $ over _tasks (L.listMoveTo 0) s
      V.EvKey (V.KChar 'G') [] ->
        M.continue $ over _tasks (L.listMoveTo $ length $ s ^. _tasks) s
      V.EvKey (V.KChar _) [] -> M.continue s
      V.EvKey _ []-> M.continue s

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent s e =
  case e of
    T.VtyEvent (V.EvKey V.KEsc []) -> M.halt s
    e -> case s ^. _page of
            CreatePage f -> createPageEvent e f
            ListPage -> listPageEvent s e

app :: M.App AppState e ()
app =
  M.App
    { M.appDraw = ui,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
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
