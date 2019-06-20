module Commands
  ( execute
  , Command(..)
  ) where

import Core (State, Event(..), Stone, Position, track)

data Command = Place Stone Position

execute :: Command -> State -> Either () (State, [Event])
execute command state = do
  events <- executeEvents command state
  pure (foldr track state events, events)

executeEvents :: Command -> State -> Either () [Event]
executeEvents (Place s p) _ = Right [StonePlaced s p]
