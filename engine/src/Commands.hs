module Commands
  ( execute
  , Command(..)
  , Error(..)
  ) where

import Core (State, Event(..), Stone, Position, track, pieceAt)

data Command
  = Place Stone Position

data Error
  = LocationAlreadyOccupied
  deriving (Eq, Show)

type CommandResult = Either Error (State, [Event])

execute :: Command -> State -> CommandResult
execute command state = do
  events <- executeEvents command state
  pure (foldr track state events, events)

executeEvents :: Command -> State -> Either Error [Event]
executeEvents (Place s p) state =
  case pieceAt p state of
    Just _ -> Left LocationAlreadyOccupied
    _ -> Right [StonePlaced s p]
