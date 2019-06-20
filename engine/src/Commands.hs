module Commands
  ( execute
  , Commmand(..)
  ) where

import Core (State, Event(..), Stone, Position, track)

data Commmand = Place Stone Position

execute :: Commmand -> State -> Either () (State, [Event])
execute command state = do
  events <- executeEvents command state
  pure (foldr track state events, events)

executeEvents :: Commmand -> State -> Either () [Event]
executeEvents (Place s p) _ = Right [StonePlaced s p]
