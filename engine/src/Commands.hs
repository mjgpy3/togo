module Commands
  ( execute
  , Command(..)
  , Error(..)
  ) where

import Core (State, Event(..), Stone, Position, track, pieceAt, turn, widthAndHeight, isEndGame)

data Command
  = Place Stone Position
  | Pass

data Error
  = LocationAlreadyOccupied
  | OutOfTurn
  | OutOfBounds
  | GameEnded
  deriving (Eq, Show)

type CommandResult = Either Error (State, [Event])

execute :: Command -> State -> CommandResult
execute command state =
  if isEndGame state
  then Left GameEnded
  else do
    events <- executeEvents command state
    pure (foldr track state events, events)

guardNotOccupied :: Position -> State -> Either Error ()
guardNotOccupied pos state =
  maybe (pure ()) (const $ Left LocationAlreadyOccupied) $ pieceAt pos state

guardTurn :: Stone -> State -> Either Error ()
guardTurn stone state =
  if turn state == stone
  then pure ()
  else Left OutOfTurn

guardInBoardBoundaries :: Position -> State -> Either Error ()
guardInBoardBoundaries (x, y) state =
  let (width, height) = widthAndHeight state
  in
    if 0 <= x && x < width && 0 <= y && y < height
    then pure ()
    else Left OutOfBounds

executeEvents :: Command -> State -> Either Error [Event]
executeEvents (Place s p) state = do
  guardNotOccupied p state
  guardTurn s state
  guardInBoardBoundaries p state
  pure [StonePlaced s p]
executeEvents Pass state =
  pure [TurnPassed]
