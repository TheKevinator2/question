{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Question where

import Data.Char (intToDigit, isLetter)
import Print
import Data.List (sort)
import OwnParse

-- A Question can be an open-ended question, a closed question where the Options offered are mutually exclusive, and a closed question where the Options are not mutually exclusive.
data Question = OpenQuestion Prompt
  | ClosedMutuallyExclusiveQuestion Prompt Options
  | ClosedQuestion Prompt Options
  deriving (Eq, Show)
-- An instance of Options is a list of Prompt's, where a Prompt is a String.
type Options = [] Option
type Option = String
-- A Prompt is a String to use in Question's
type Prompt = String
-- An answer is just a String: for an open-ended question, a String is suitable; for a close-ended question, the String is processed where needed (e.g. the system needs to use an Answer to decide the follow-up question).
type Answer = String

instance Export Question where
  export (OpenQuestion p) = " o" ++ " `" ++ export p ++ "`\n"
  export (ClosedQuestion p opt) = " c" ++ " `" ++ export p ++ "`" ++ " [" ++ export opt ++ "]\n"
  export (ClosedMutuallyExclusiveQuestion p opt) = " m" ++ " `" ++ export p ++ "`" ++ " [" ++ export opt ++ "]\n"

instance Export Options where
  export l = foldr (++) "" $ map (flip (++) ";") l

instance Export String where
  export = id

emptyQ :: Question
emptyQ = ClosedQuestion "" []

prompt :: Question -> Prompt
prompt (ClosedMutuallyExclusiveQuestion p _) = p
prompt (ClosedQuestion p _) = p
prompt (OpenQuestion p) = p

options :: Question -> Options
options (ClosedMutuallyExclusiveQuestion _ opt) = opt
options (ClosedQuestion _ opt) = opt
options (OpenQuestion _) = []

instance Ask Question where
  ask (OpenQuestion p) = p ++ "\n"
  ask (ClosedMutuallyExclusiveQuestion p opt) = p ++ "\n" ++ prepare opt 1
  ask (ClosedQuestion p opt) = p ++ "\n" ++ prepare opt 1

-- This typeclass is used to format Options into a String that enumerates the questions in Options.
class Prepare a where
  prepare :: a -> Int -> String

instance Prepare Options where
  prepare [] n = ""
  prepare (x : xs) n = [(intToDigit n)] ++ ". " ++ x ++ "\n" ++ prepare xs (n + 1)

-- DSL for constructing Questions

openQ :: Prompt -> Question
openQ p = OpenQuestion p

closedQ :: Prompt -> Options -> Question
closedQ p opt = ClosedQuestion (p ++ " You can choose multiple options.") opt

closedMUQ :: Prompt -> Options -> Question
closedMUQ p opt = ClosedMutuallyExclusiveQuestion (p ++ " These options are mutually exclusive.") opt

closedMUQP :: Prompt -> Options -> Question
closedMUQP p opt = ClosedMutuallyExclusiveQuestion (p) opt

closedQP :: Prompt -> Options -> Question
closedQP p opt = ClosedQuestion (p) opt

linearScaleQ :: Prompt -> Int -> Question
linearScaleQ p upper = ClosedMutuallyExclusiveQuestion p (map show [1,2..upper])

-- Question Parsers

isType :: Char -> Bool
isType x = (x == 'o') || (x == 'c') || (x == 'm')

mkQuestion :: Parser Question
mkQuestion = do
  t <- token (sat isType)
  p <- token pr
  case t of
    'o' -> return (openQ p)
    'c' -> do
      opt <- op
      case last opt of
        "" -> return (closedQP p (init opt))
        otherwise -> return (closedQP p opt)
    'm' -> do
      opt <- op
      case last opt of
        "" -> return (closedMUQP p (init opt))
        otherwise -> return (closedMUQP p opt)

pr :: Parser String
pr = do
  prompt <- bracket (char '`') (many sent) (char '`')
  return prompt

op :: Parser Options
op = do
  opt <- op2
  return opt

op2 :: Parser [String]
op2 = do
  opt <- bracket (char '[') op3 (char ']')
  return opt

op3 :: Parser [String]
op3 = do
  opt <- sepBy1 (many sent) (char ';')
  return opt
