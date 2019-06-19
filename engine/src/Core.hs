module Core
  ( summarize
  , emptyGame
  ) where

import qualified Data.Map.Strict as M

data Stone = Black | White deriving Eq

data Event = Event
type State = M.Map (Int, Int) Stone

emptyGame = M.empty

summarize :: [Event] -> State
summarize = const emptyGame
