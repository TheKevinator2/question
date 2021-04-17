module Main where

import Data.Typeable

import QDSL
import Question
import OwnParse
import Print
import Tui
import EditorTuiDraw
import EditorTuiState
import Util
-- System
import System.Environment (getArgs)
import System.IO
import System.Exit

main :: IO ()
main = do
  x <- getArgs
  case safeHead $ tail x of
    Nothing -> case safeHead x of
      Just "answer" -> tui questionnaire
      Just "edit" -> editorTui questionnaire
      otherwise -> die "Please supply either 'answer' or 'edit' as the first argument."

    Just fn -> do
      (handle, qep) <- importFile fn
      case safeHead x of
        Just "answer" -> case qep of
          Nothing -> tui questionnaire
          Just qe -> tui qe
        Just "edit" -> case qep of
          Nothing -> editorTui questionnaire
          Just qe -> editorTui qe
        otherwise -> die "Please supply either 'answer' or 'edit' as the first argument."
      hClose handle
  return ()

importFile :: String -> IO (Handle, Maybe QuestionnaireElement)
importFile str = do
  handle <- openFile str ReadMode
  x <- hGetContents handle
  qe <- importSeqQuestionnaire (concat $ lines x)
  return (handle, qe)
