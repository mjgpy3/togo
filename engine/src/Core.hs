module Core
  ( summarize
  , emptyGame
  , gameOf
  , Stone(..)
  , Event(..)
  ) where

import qualified Data.Map.Strict as M

data Stone = Black | White deriving (Eq, Show)

data Event = StonePlaced Stone (Int, Int)
type State = M.Map (Int, Int) Stone

emptyGame :: State
emptyGame = M.empty

gameOf :: [((Int, Int), Stone)] -> State
gameOf = M.fromList

summarize :: [Event] -> State
summarize [] = emptyGame
summarize (StonePlaced color pos:rest) =
  M.insert pos color $ summarize rest
