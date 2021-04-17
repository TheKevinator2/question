{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module EditorTuiState where

import QDSL
import Question
import Print
import Util
-- IO
import System.IO
-- liftIO
import Control.Monad.IO.Class (liftIO)
-- Lens
import Lens.Micro
import Lens.Micro.Extras
import Lens.Micro.TH
-- Cursor
import Cursor.List
-- Brick
import qualified Brick.Main as M
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Edit
-- Vty
import Graphics.Vty.Input.Events

data EditorTuiState =
  EditorTuiState
    { _currentQ :: QuestionnaireElement
    , _prevQ :: [] QuestionnaireElement
    , _qeCursor :: ListCursor QuestionnaireElement
    , _edit :: Editor String ResourceName
    , _mode :: Mode
    , _editPrompt :: Prompt
    , _editOptions :: Options
  }
  deriving (Show)

data Mode =
  E |
  O |
  C EditMode |
  CMU EditMode
  deriving (Show, Eq)

data EditMode =
  P |
  Opt
  deriving (Show, Eq)

data ResourceName =
  VP1 |
  AnswerEditor
  deriving (Show, Eq, Ord)

makeLenses ''EditorTuiState

scrollUp :: EditorTuiState -> EditorTuiState
scrollUp s = case listCursorSelectPrev $ s^.qeCursor of
  Nothing -> s
  Just c' -> (qeCursor .~ c') s

scrollDown :: EditorTuiState -> EditorTuiState
scrollDown s = case listCursorSelectNext $ s^.qeCursor of
  Nothing -> s
  Just c' -> case listCursorNext c' of
    [] -> s
    otherwise -> (qeCursor .~ c') s

currentQE :: ListCursor QuestionnaireElement -> QuestionnaireElement
currentQE = head . listCursorNext

prevQE :: EditorTuiState -> QuestionnaireElement
prevQE s = head $ s^.prevQ

selectPrevQE :: EditorTuiState -> EditorTuiState
selectPrevQE s = case null $ s^.prevQ of
  True -> s
  False -> let
    newCurrentQE = head $ s^.prevQ
    newPrevQE = drop 1 $ s^.prevQ
    newQECursor = makeListCursor $ newCurrentQE^.next
    in s
    & currentQ .~ newCurrentQE
    & prevQ .~ newPrevQE
    & qeCursor .~ newQECursor

selectCurrentQE :: EditorTuiState -> EditorTuiState
selectCurrentQE s = case null $ s^.currentQ.next of
  True -> s
  False -> let
    newCurrentQE = currentQE $ s^.qeCursor
    newPrevQE = s^.currentQ : s^.prevQ
    newQECursor = makeListCursor $ newCurrentQE^.next
    in s
    & currentQ .~ newCurrentQE
    & prevQ .~ newPrevQE
    & qeCursor .~ newQECursor

addQE :: EditorTuiState -> QuestionnaireElement -> EditorTuiState
addQE s nqe = let
  newCurrentQE = addFQ nqe $ s^.currentQ
  in s
  & currentQ .~ newCurrentQE

openEMode :: EditorTuiState -> EditorTuiState
openEMode s = s & mode .~ E & clearEditMode

openOMode :: EditorTuiState -> EditorTuiState
openOMode s = s & mode .~ O & edit .~ emptyAnswerEditor

openCMode :: EditorTuiState -> EditorTuiState
openCMode s = s & mode .~ (C P) & edit .~ emptyAnswerEditor

openCOptMode :: EditorTuiState -> EditorTuiState
openCOptMode s = s & mode .~ (C Opt) & edit .~ emptyAnswerEditor

openCMUMode :: EditorTuiState -> EditorTuiState
openCMUMode s = s & mode .~ (CMU P) & edit .~ emptyAnswerEditor

openCMUOptMode :: EditorTuiState -> EditorTuiState
openCMUOptMode s = s & mode .~ (CMU Opt) & edit .~ emptyAnswerEditor

emptyAnswerEditor :: Editor String ResourceName
emptyAnswerEditor = editor AnswerEditor Nothing ""

clearEditor :: EditorTuiState -> EditorTuiState
clearEditor s = s & edit .~ emptyAnswerEditor

extractEditContents :: Editor String ResourceName -> String
extractEditContents = foldr (++) "" . getEditContents

addOpenQE :: EditorTuiState -> EditorTuiState
addOpenQE s = case null $ s^.prevQ of
  True -> let
    nqe = mkOQE s
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & qeCursor .~ nqec
  False -> let
    nqe = mkOQE s
    pqe = prevQE s
    npqe = setPrevQLinks (s^.prevQ) nqe
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & prevQ .~ npqe
    & qeCursor .~ nqec

mkOQE :: EditorTuiState -> QuestionnaireElement
mkOQE s = let
  qe = s^.currentQ
  oqe = openQE $ s^.editPrompt
  in addFQ oqe qe

addPrompt :: EditorTuiState -> Prompt -> EditorTuiState
addPrompt s p = s & editPrompt .~ p

addEditOption :: EditorTuiState -> Option -> EditorTuiState
addEditOption s opt = s & editOptions %~ ((++) [opt])

setPrevQLinks :: [QuestionnaireElement] -> QuestionnaireElement -> [QuestionnaireElement]
setPrevQLinks l qe = updateLoop l qe

addClosedQE :: EditorTuiState -> EditorTuiState
addClosedQE s = case null $ s^.prevQ of
  True -> let
    nqe = mkCQE s
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & qeCursor .~ nqec
  otherwise -> let
    nqe = mkCQE s
    npqe = setPrevQLinks (s^.prevQ) nqe
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & prevQ .~ npqe
    & qeCursor .~ nqec

mkCQE :: EditorTuiState -> QuestionnaireElement
mkCQE s = let
  qe = s^.currentQ
  p = s^.editPrompt
  opt = s^.editOptions
  cqe = closedQE p opt
  in addFQ cqe qe

addClosedMUQE :: EditorTuiState -> EditorTuiState
addClosedMUQE s = case null $ s^.prevQ of
  True -> let
    nqe = mkCMUQE s
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & qeCursor .~ nqec
  False -> let
    nqe = mkCMUQE s
    npqe = setPrevQLinks (s^.prevQ) nqe
    nqec = makeListCursor $ nqe^.next
    in s
    & currentQ .~ nqe
    & prevQ .~ npqe
    & qeCursor .~ nqec

mkCMUQE :: EditorTuiState -> QuestionnaireElement
mkCMUQE s = let
  qe = s^.currentQ
  p = s^.editPrompt
  opt = s^.editOptions
  cqe = closedMUQE p opt
  in addFQ cqe qe

removeQE :: EditorTuiState -> EditorTuiState
removeQE s = case listCursorNextItem $ s^.qeCursor of
  Nothing -> s
  Just rqe -> case safeHead $ s^.prevQ of
    Nothing -> let
      qe = s^.currentQ
      nqe = removeFQ rqe qe
      nqec = makeListCursor $ nqe^.next
      in s
      & currentQ .~ nqe
      & qeCursor .~ nqec
    Just pqe -> let
      qe = s^.currentQ
      nqe = removeFQ rqe qe
      npqe = setPrevQLinks (s^.prevQ) nqe
      nqec = makeListCursor $ nqe^.next
      in s
      & currentQ .~ nqe
      & prevQ .~ npqe
      & qeCursor .~ nqec

clearEditMode :: EditorTuiState -> EditorTuiState
clearEditMode s = s
  & edit .~ emptyAnswerEditor
  & editPrompt .~ ""
  & editOptions .~ []

saveQuestionnaire :: EditorTuiState -> IO EditorTuiState
saveQuestionnaire s = do
  handle <- openFile "questionnaire.txt" ReadWriteMode
  hPutStr handle $ export $ rootQE s
  hClose handle
  return s

rootQE :: EditorTuiState -> QuestionnaireElement
rootQE s = case safeLast $ s^.prevQ of
  Nothing -> s^.currentQ
  Just root -> root

handleEditorTuiEvent :: EditorTuiState -> BrickEvent ResourceName e -> EventM ResourceName (Next EditorTuiState)
handleEditorTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey KEsc [] -> case s^.mode of
          E -> M.halt s
          otherwise -> M.continue $ openEMode s

        EvKey KUp [] -> case s^.mode of
          E -> M.continue $ scrollUp s
          otherwise -> M.continue s

        EvKey KDown [] -> case s^.mode of
          E -> M.continue $ scrollDown s
          otherwise -> M.continue s

        EvKey (KChar '`') [] -> case s^.mode of
          O -> M.continue $ openEMode $ addOpenQE s
          C Opt -> M.continue $ openEMode $ addClosedQE s
          CMU Opt -> M.continue $ openEMode $ addClosedMUQE s
          otherwise -> M.continue s


        EvKey KEnter [] -> case s^.mode of
          E -> M.continue $ selectCurrentQE s
          O -> M.continue $ addPrompt s $ extractEditContents $ s^.edit
          C em -> case em of
            P -> M.continue $ openCOptMode $ addPrompt s $ extractEditContents $ s^.edit
            Opt -> M.continue $ clearEditor $ addEditOption s $ extractEditContents $ s^.edit
          CMU em -> case em of
            P -> M.continue $ openCMUOptMode $ addPrompt s $ extractEditContents $ s^.edit
            Opt -> M.continue $ clearEditor $ addEditOption s $ extractEditContents $ s^.edit

        EvKey KBS [] -> case s^.mode of
          E -> M.continue $ selectPrevQE s
          otherwise -> M.continue =<< handleEventLensed s edit handleEditorEvent vtye

        EvKey KDel [] -> case s^.mode of
          E -> M.continue $ removeQE s
          otherwise -> M.continue s

        -- EvKey (KChar 'i') [MCtrl] -> case s^.mode of
        --   E -> M.continue $ inspectQE s
        --   otherwise -> M.continue s

        EvKey (KChar 'r') [MCtrl] -> M.continue $ openOMode s

        EvKey (KChar 'e') [MCtrl] -> M.continue $ openEMode s

        EvKey (KChar 'q') [MCtrl] -> M.continue $ openCMode s

        EvKey (KChar 'w') [MCtrl] -> M.continue $ openCMUMode s

        EvKey (KChar 's') [MCtrl] -> (liftIO $ saveQuestionnaire s) >>= M.continue

        _ -> case s^.mode of
          E -> M.continue s
          otherwise -> M.continue =<< handleEventLensed s edit handleEditorEvent vtye

    _ -> M.continue s
