module Print where

class Ask a where
  ask :: a -> String

class Export a where
  export :: a -> String
