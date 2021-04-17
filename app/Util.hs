module Util where

safeHead :: [a] -> Maybe a
safeHead l = case null l of
  True -> Nothing
  False -> Just $ head l

safeLast :: [a] -> Maybe a
safeLast l = case null l of
  True -> Nothing
  False -> Just (last l)
