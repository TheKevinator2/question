{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Tui where

import QDSL
import Print
import Question

import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.TH
import System.IO

-- TUI
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Cursor.List

data ResourceName =
  ResourceName |
  AnswerEditor |
  VP1
  deriving (Show, Eq, Ord)

data TuiState =
  TuiState
    { _prevQ :: [QuestionnaireElement]
    , _currentQ :: QuestionnaireElement
    , _optCursor :: ListCursor Option
    , _edit :: Editor String ResourceName
  }
  deriving (Show)

makeLenses ''TuiState

tui :: QuestionnaireElement -> IO ()
tui qe = do
  initialState <- buildInitialState qe
  endState <- defaultMain tuiApp initialState
  saveAnswer $ endState^.prevQ
  print endState

saveAnswer :: [QuestionnaireElement] -> IO ()
saveAnswer l = do
  handle <- openFile "answer.txt" ReadWriteMode
  hPutStr handle $ unlines $ number $ map ((++) ". " . show) l
  hClose handle
  return ()

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", blue `on` white)]
    }

buildInitialState :: QuestionnaireElement -> IO TuiState
buildInitialState qe = pure $ TuiState { _prevQ = [], _currentQ = qe, _optCursor = makeListCursor $ getQOptions qe, _edit = emptyAnswerEditor }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [viewport VP1 Vertical $ vLimit 100000000 $ vBox $ drawPrevQEWidget ts ++ drawCurrentQEWidget ts]

drawPrevQEWidget :: TuiState -> [Widget ResourceName]
drawPrevQEWidget ts = [vBox $ map drawQE $ ts^.prevQ]

drawCurrentQEWidget :: TuiState -> [Widget ResourceName]
drawCurrentQEWidget ts = [(drawCurrentQE ts)]

-- There is a distinction between drawing 'current' and other QuestionnaireElement's, because of the fact that Option selection highlighting should only be for the Question that is being answered at that point in time
drawCurrentQE :: TuiState -> Widget ResourceName
drawCurrentQE ts = let
  c = ts^.optCursor
  qe = ts^.currentQ
  ed = ts^.edit
  in case qe^.question of
    OpenQuestion p -> vBox [drawCurrentQuestion c $ qe^.question, drawEditor ed]
    otherwise -> (drawCurrentQuestion c) $ qe^.question

drawQE :: QuestionnaireElement -> Widget ResourceName
drawQE qe = border $ vBox $ [drawQuestion $ qe^.question, drawAnswer $ (++) "You answered: " $ qe^.answerPH]

drawCurrentQuestion :: ListCursor Option -> Question -> Widget ResourceName
drawCurrentQuestion c q = border $ vBox [drawPrompt $ prompt q, (drawCurrentOptions c)]

drawQuestion :: Question -> Widget ResourceName
drawQuestion q = vBox [drawPrompt $ prompt q, (drawOptions $ options q)]

drawPrompt :: Prompt -> Widget ResourceName
drawPrompt p = padBottom (Pad 1) $ strWrap p

drawCurrentOptions :: ListCursor Option -> Widget ResourceName
drawCurrentOptions c = case listCursorNext c of
  [] -> case listCursorPrev c of
    -- if there are no options (open question)
    [] -> emptyWidget
    -- if last option has been reached
    otherwise -> vBox $ concat [
          map (drawOption False) $ reverse $ drop 1 $ listCursorPrev c
        , [(drawOption True) $ head $ listCursorPrev c]
      ]
  otherwise -> vBox $ concat [
        map (drawOption False) $ reverse $ listCursorPrev c
      , [(drawOption True) $ head $ listCursorNext c]
      , map (drawOption False) $ drop 1 $ listCursorNext c
    ]

drawOptions :: Options -> Widget ResourceName
drawOptions opt = vBox $ map (drawOption False) opt

drawOption :: Bool -> Option -> Widget ResourceName
drawOption selected =
  (if selected
    then withAttr "selected"
    else id) .
  str

drawAnswer :: Answer -> Widget ResourceName
drawAnswer ans = padTop (Pad 1) $ str ans

drawEditor :: Editor String ResourceName -> Widget ResourceName
drawEditor = renderEditor (str . unlines) True

drawEnd :: Widget ResourceName
drawEnd = border $ strWrap "Congratulations, you have reached the end of this questionnaire!"

currentOption :: ListCursor Option -> Option
currentOption = head . listCursorNext

filterNewline :: String -> String
filterNewline = filter (/= '\n')

emptyAnswerEditor :: Editor String ResourceName
emptyAnswerEditor = editor AnswerEditor Nothing ""

scrollUpOpt :: TuiState -> TuiState
scrollUpOpt s = case listCursorSelectPrev $ s^.optCursor of
  Nothing -> s
  Just c' -> (optCursor .~ c') s

scrollDownOpt :: TuiState -> TuiState
scrollDownOpt s = case listCursorSelectNext $ s^.optCursor of
  Nothing -> s
  Just c' -> case listCursorNext c' of
    [] -> s
    otherwise -> (optCursor .~ c') s

vp1Scroll :: ViewportScroll ResourceName
vp1Scroll = viewportScroll VP1

progressQuestionnaire :: TuiState -> TuiState
progressQuestionnaire s =
  let
    nextQ = QDSL.continue (s^.currentQ)
    in s
    & prevQ .~ (s^.prevQ ++ [s^.currentQ])
    & currentQ .~ nextQ
    & optCursor .~ (makeListCursor $ getQOptions nextQ)
    & edit .~ emptyAnswerEditor

setPrevQLinks :: [QuestionnaireElement] -> QuestionnaireElement -> [QuestionnaireElement]
setPrevQLinks l qe = reverse $ replaceLoop (reverse $ l) qe

chooseCurrentOption :: TuiState -> TuiState
chooseCurrentOption s = let
  newCurrentQE = submit (s^.currentQ) (filterNewline $ currentOption $ s^.optCursor)
  newPrevQE = setPrevQLinks (s^.prevQ) newCurrentQE
  in s
  & prevQ .~ newPrevQE
  & currentQ .~ newCurrentQE

addCurrentOption :: TuiState -> TuiState
addCurrentOption s = let
  newCurrentQE = addAnswer (s^.currentQ) (filterNewline $ currentOption $ s^.optCursor)
  newPrevQE = setPrevQLinks (s^.prevQ) newCurrentQE
  in s
  & prevQ .~ newPrevQE
  & currentQ .~ newCurrentQE

submitFreeAnswer :: TuiState -> TuiState
submitFreeAnswer s = let
  newCurrentQE = submit (s^.currentQ) (foldr (++) "" $ getEditContents $ s^.edit)
  newPrevQE = setPrevQLinks (s^.prevQ) newCurrentQE
  in s
  & prevQ .~ newPrevQE
  & currentQ .~ newCurrentQE

handleTuiEvent :: TuiState -> BrickEvent ResourceName e -> EventM ResourceName (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey KEsc [] -> halt s

        EvKey KUp [] -> Brick.Main.continue $ scrollUpOpt s

        EvKey KDown [] -> Brick.Main.continue $ scrollDownOpt s

        EvKey KUp [MCtrl] -> vScrollBy vp1Scroll (-1) >> Brick.Main.continue s

        EvKey KLeft [MCtrl] -> vScrollBy vp1Scroll (-5) >> Brick.Main.continue s

        EvKey KDown [MCtrl] -> vScrollBy vp1Scroll 1 >> Brick.Main.continue s

        EvKey KRight [MCtrl] -> vScrollBy vp1Scroll 5 >> Brick.Main.continue s

        EvKey (KChar '`') [] -> if hasFollowup $ s^.currentQ
          then Brick.Main.continue $ progressQuestionnaire s
          else Brick.Main.continue s

        EvKey (KEnter) [] -> case s^.currentQ.question of
          ClosedMutuallyExclusiveQuestion _ _ ->
            Brick.Main.continue $ chooseCurrentOption s
          ClosedQuestion _ _ ->
            Brick.Main.continue $ addCurrentOption s
          OpenQuestion _ ->
            Brick.Main.continue $ submitFreeAnswer s

        _ -> Brick.Main.continue =<< handleEventLensed s edit handleEditorEvent vtye

    _ -> Brick.Main.continue s

-- Util

number :: [String] -> [String]
number =
  let
    intList = map show [1..]
    in zipWith (++) intList
