{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

module QDSL where

import Data.List
import Question
import Print
import OwnParse
import Util

import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.TH

data QuestionnaireElement =
    Q
      { _question :: Question
      , _answerPH :: Answer
      , _next :: [] QuestionnaireElement
      -- , _prev :: Maybe QuestionnaireElement
    } --deriving (Show)

makeLenses ''QuestionnaireElement

qe :: Question -> QuestionnaireElement
qe q = Q q "" []

closedMUQE :: Prompt -> Options -> QuestionnaireElement
closedMUQE p opt = Q (closedMUQ p opt) "" []

closedQE :: Prompt -> Options -> QuestionnaireElement
closedQE p opt = Q (closedQ p opt) "" []

openQE :: Prompt -> QuestionnaireElement
openQE p = Q (openQ p) "" []

emptyQE :: QuestionnaireElement
emptyQE = Q emptyQ "" []

getQOptions :: QuestionnaireElement -> Options
getQOptions qe = options $ qe^.question

getPrompt :: QuestionnaireElement -> Prompt
getPrompt qe = prompt $ qe^.question

type Questionnaire = QuestionnaireElement

instance Eq QuestionnaireElement where
  (==) qe1 qe2 = qe1^.question == qe2^.question

instance Ask QuestionnaireElement where
  ask qe = ask $ qe^.question

instance Ask [QuestionnaireElement] where
  ask = concat . map ask

instance Show QuestionnaireElement where
  show qe = ask qe ++ "\n" ++ ("Answer: " ++ qe^.answerPH ++ "\n")

-- TODO: actually implement this
continue :: QuestionnaireElement -> QuestionnaireElement
continue qe = (qe^.next)!!0

hasFollowup :: QuestionnaireElement -> Bool
hasFollowup qe = not $ null $ qe^.next

instance Export QuestionnaireElement where
  export = export2 "" 0

export2 :: String -> Int -> QuestionnaireElement -> String
export2 str n qe = let
  numbering = str ++ show n ++ "."
  q = numbering ++ (export $ qe^.question)
  fq = (concat . map (export2 numbering (n + 1))) $ qe^.next
  in q ++ fq

importSeqQuestionnaire :: String -> IO (Maybe QuestionnaireElement)
importSeqQuestionnaire str = let
  qel = parse mkSeqQuestionnaire $ str
  in case safeHead qel of
    Nothing -> return $ Nothing
    Just qep -> return $ Just $ fst $ qep

mkSeqQuestionnaire :: Parser QuestionnaireElement
mkSeqQuestionnaire = do
  qe <- mkQE
  qes <- many mkQE
  return $ linkSeqQuestionnaire (qe:qes)

mkQE :: Parser QuestionnaireElement
mkQE = do
  heading
  q <- mkQuestion
  return (qe q)

heading :: Parser [[Char]]
heading = do
  front (return "")
  x <- token num
  return x

num :: Parser [[Char]]
num = do
  x <- sepBy (many digit) (char '.')
  return x

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

-- Typeclass to submit an Answer to a QuestionnaireElement
class Submit a where
  submit :: a -> String -> a

instance Submit QuestionnaireElement where
  submit qe str = (answerPH .~ str) qe

addAnswer :: QuestionnaireElement -> String -> QuestionnaireElement
addAnswer qe str = if null $ qe^.answerPH
  then (answerPH .~ str) qe
  else let
    t1 = T.pack $ qe^.answerPH
    t2 = T.pack str
    f = (++ (", " ++ str))
    in if T.isInfixOf t2 t1
      then qe
      else (answerPH %~ f) qe

-- Add a potential follow-up question to the given question
addFollowupQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
addFollowupQ qe follow = (next %~ (++ [follow])) qe

addFQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
addFQ q2 q1 = addFollowupQ q1 q2

-- Remove a follow-up question to the given question
removeFollowupQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
removeFollowupQ qe follow = (next %~ (delete follow)) qe

removeFQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
removeFQ q2 q1 = removeFollowupQ q1 q2

clearFollowupQ :: QuestionnaireElement -> QuestionnaireElement
clearFollowupQ qe = (next .~ []) qe

replaceFollowupQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
replaceFollowupQ q1 q2 = addFollowupQ (clearFollowupQ q1) q2

-- Add an extra Option to the given QuestionnaireElement
addOption :: QuestionnaireElement -> Option -> QuestionnaireElement
addOption (Q (ClosedMutuallyExclusiveQuestion p opt) a l) option = Q (ClosedMutuallyExclusiveQuestion p (opt ++ [option])) a l
addOption (Q (ClosedQuestion p opt) a l) option = Q (ClosedQuestion p (opt ++ [option])) a l
addOption (Q (OpenQuestion p) ans l) option = (Q (OpenQuestion p) ans l)

replaceLoop :: [QuestionnaireElement] -> QuestionnaireElement -> [QuestionnaireElement]
replaceLoop [] _ = []
replaceLoop (q:qs) q1 = let
  newQ = replaceFollowupQ q q1
  in newQ : replaceLoop qs newQ

updateFQ :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
updateFQ q1 q = addFQ q1 $ removeFQ q1 q

updateLoop :: [QuestionnaireElement] -> QuestionnaireElement -> [QuestionnaireElement]
updateLoop [] _ = []
updateLoop (q:qs) q1 = let
  newQ = updateFQ q1 q
  in newQ : updateLoop qs newQ

-- Sample questions

closedQuestionTest :: Question
closedQuestionTest = closedQ "What game(s) do you prefer?" ("League Of Legends" : "Factorio" : "Amazing Cultivation Simulator" : [])

closedQuestionnaireElement :: QuestionnaireElement
closedQuestionnaireElement = Q closedQuestionTest "" []

closedMUQuestionTest :: Question
closedMUQuestionTest = closedMUQ "What is your favorite way of greeting someone?" ("hey" : "hallo" : "hoi" : [])

closedMUQuestionnaireElement :: QuestionnaireElement
closedMUQuestionnaireElement = Q closedMUQuestionTest "This string is shown if there is no answer." []

openQuestionTest :: Question
openQuestionTest = openQ "What do you think about Haskell?"

openQuestionnaireElement :: QuestionnaireElement
openQuestionnaireElement = Q openQuestionTest "" []

endQ :: QuestionnaireElement
endQ = Q (openQ "Congratulations, you have reached the end of this questionnaire!") "" []

-- Sample questionnaires

questionnaire :: QuestionnaireElement
questionnaire = closedQuestionnaireElement
  & addFQ (addFQ (addFQ (addFQ endQ openQuestionnaireElement) openQuestionnaireElement) closedMUQuestionnaireElement)
  & addFQ (addFQ endQ openQuestionnaireElement)

-- Utility functions

linkNElement :: QuestionnaireElement -> QuestionnaireElement -> Int -> [QuestionnaireElement]
linkNElement q1 _ 0 = [q1]
linkNElement q1 q2 n = if hasFollowup q1
  then q1 : linkNElement (continue q1) q2 n
  else q1 : linkNElement (continue $ addFollowupQ q1 q2) q2 (n - 1)

flattenQE :: [QuestionnaireElement] -> QuestionnaireElement
flattenQE [q] = q
flattenQE (x:xs) = addFollowupQ (clearFollowupQ x) (flattenQE xs)

linkSeqQuestionnaire :: [QuestionnaireElement] -> QuestionnaireElement
linkSeqQuestionnaire [q] = q
linkSeqQuestionnaire (q:qs) = addFollowupQ q (linkSeqQuestionnaire qs)

-- |This function adds a given 'QuestionnaireElement' to the end of a sequential questionnaire, for a given amount of times
addNElement :: QuestionnaireElement -> QuestionnaireElement -> Int -> QuestionnaireElement
addNElement q1 q2 n = flattenQE $ linkNElement q1 q2 n

addFinalQE :: QuestionnaireElement -> QuestionnaireElement -> QuestionnaireElement
addFinalQE q1 q2 = addNElement q1 q2 1

parseTest :: String
parseTest = "0. c `What games do you prefer? You can choose multiple options.` [League Of Legends;Factorio;Amazing Cultivation Simulator;] 0.1. m `What is your favorite way of greeting someone? These options are mutually exclusive.` [hey;hallo;hoi;] 0.1.2. o `What do you think about Haskell?` 0.1. o `What do you think about Haskell?`"

parseTestShort :: String
parseTestShort = "0. c `What games do you prefer? You can choose multiple options.` [League Of Legends;Factorio;Amazing Cultivation Simulator;]"

test2 :: String
test2 = " 0.1. m `What is your favorite way of greeting someone? These options are mutually exclusive.` [hey;hallo;hoi;] 0.1.2. o `What do you think about Haskell?` 0.1. o `What do you think about Haskell?`"
