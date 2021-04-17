{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module EditorTuiDraw where

import QDSL
import Question
import Print
import EditorTuiState
import Util

import qualified Data.Text as T
-- Lens
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
-- Cursor
import Cursor.List
-- Brick
import Brick.AttrMap
import qualified Brick.Main as M
import Brick.Types
import Brick.Util
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import Brick.Widgets.Edit
-- Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

editorTui :: QuestionnaireElement -> IO ()
editorTui qe = do
  initialState <- buildInitialState qe
  endState <- M.defaultMain editorTuiApp initialState
  print endState

editorTuiApp :: M.App EditorTuiState e ResourceName
editorTuiApp =
  M.App
    { M.appDraw = drawEditorTui
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = handleEditorTuiEvent
    , M.appStartEvent = pure
    , M.appAttrMap = const $ attrMap mempty [("selected", blue `on` white)]
    }

buildInitialState :: QuestionnaireElement -> IO EditorTuiState
buildInitialState qe = pure $ EditorTuiState { _currentQ = qe, _prevQ = [], _qeCursor = makeListCursor $ qe^.next, _edit = emptyAnswerEditor, _mode = E, _editPrompt = "", _editOptions = []}

drawEditorTui :: EditorTuiState -> [Widget ResourceName]
drawEditorTui s = case s^.mode of
  E -> drawExplorer s
  O -> drawEditorOQE s
  C _ -> drawEditorCQE s
  CMU _ -> drawEditorCQE s

drawExplorer :: EditorTuiState -> [Widget ResourceName]
drawExplorer s =
  let
    pqe = safeHead $ s^.prevQ
    qe = s^.currentQ
    l = qe^.next
    prevQE = B.borderWithLabel (str "Previous questionnaire element: ") $ drawPrevQE pqe
    currentQE = B.borderWithLabel (str "Current questionnaire element: ") $ drawQE qe
    followingQE = B.borderWithLabel (str "Following questionnaire elements: ") $ drawQECursor l $ s^.qeCursor
    in [viewport VP1 Vertical $ vBox [prevQE, currentQE, followingQE]]

drawEditorOQE :: EditorTuiState -> [Widget ResourceName]
drawEditorOQE = drawEditorPrompt

drawEditorCQE :: EditorTuiState -> [Widget ResourceName]
drawEditorCQE s = case s^.mode of
  C P -> drawEditorPrompt s
  CMU P -> drawEditorPrompt s
  C Opt -> drawEditorOptions s
  CMU Opt -> drawEditorOptions s

drawEditorPrompt :: EditorTuiState -> [Widget ResourceName]
drawEditorPrompt s = let
  heading = str "Type in the desired prompt for the open question: "
  ed = s^.edit
  in [vBox [B.border $ heading, drawEditor ed]]

drawEditorOptions :: EditorTuiState -> [Widget ResourceName]
drawEditorOptions s = let
  promptHeading = str "This is the prompt you have entered: "
  prompt = s^.editPrompt
  promptWidget = B.border $ vBox [promptHeading, (drawPrompt False) prompt]
  optionHeading = str "These are the options you have already entered: "
  options = s^.editOptions
  optionsWidget = B.border $ vBox [optionHeading, drawOptions options]
  heading = str "Type in the desired option for the closed question: "
  ed = s^.edit
  in [vBox [promptWidget, optionsWidget, B.border $ heading, drawEditor ed]]

drawQE :: QuestionnaireElement -> Widget ResourceName
drawQE qe =
  let
    q = qe^.question
    in vBox [B.border $ drawQuestion q]

drawPrevQE :: Maybe QuestionnaireElement -> Widget ResourceName
drawPrevQE qe = case qe of
  Nothing -> str "There is no previous questionnaire element."
  Just qe' -> let
    q = qe'^.question
    in vBox [drawPrompt False $ prompt q]

drawQECursor :: [QuestionnaireElement] -> ListCursor QuestionnaireElement -> Widget ResourceName
drawQECursor l c =
  let
    intList = map show [1..]
    in case listCursorNext c of
      [] -> case listCursorPrev c of
        [] -> emptyWidget
        otherwise -> vBox $ concat [
              map (drawPrompt False . getPrompt) $ reverse $ drop 1 $ listCursorPrev c
            , [(drawPrompt True . getPrompt) $ head $ listCursorPrev c]
          ]
      otherwise -> vBox $ concat [
            map (drawPrompt False . getPrompt) $ reverse $ listCursorPrev c
          , [(drawPrompt True . getPrompt) $ head $ listCursorNext c]
          , map (drawPrompt False . getPrompt) $ drop 1 $ listCursorNext c
        ]

drawQuestion :: Question -> Widget ResourceName
drawQuestion q = vBox [drawPrompt False $ prompt q, (drawOptions $ options q)]

drawPrompt :: Bool -> Prompt -> Widget ResourceName
drawPrompt selected =
  (if selected
    then withAttr "selected"
    else id) .
  padBottom (Pad 0) . B.border . strWrap

drawOptions :: Options -> Widget ResourceName
drawOptions opt = vBox $ map (drawOption False) $ number $ map ((++) ". ") opt

drawOption :: Bool -> Option -> Widget ResourceName
drawOption selected =
  (if selected
    then withAttr "selected"
    else id) .
  strWrap

drawEditor :: Editor String ResourceName -> Widget ResourceName
drawEditor ed = renderEditor (str . unlines) True ed

-- Util

number :: [String] -> [String]
number =
  let
    intList = map show [1..]
    in zipWith (++) intList
