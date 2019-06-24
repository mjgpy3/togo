{-# LANGUAGE NamedFieldPuns #-}

module Commands
  ( execute
  , Command(..)
  , Error(..)
  ) where

import Core (State, Event(..), Stone, Position(..), occupied, turn, size, isEndGame, wouldNotHaveLiberties)

data Command
  = Place Stone Position
  | Pass
  | Resign

data Error
  = LocationAlreadyOccupied
  | OutOfTurn
  | OutOfBounds
  | GameEnded
  | PlacementHasNoLiberties
  deriving (Eq, Show)

type CommandResult = Either Error Event

execute :: Command -> State -> CommandResult
execute command state =
  if isEndGame state
  then Left GameEnded
  else execute' command state

thrownWhen :: Error -> Bool -> Either Error ()
thrownWhen err condition =
  if condition
  then Left err
  else pure ()

guardNotOccupied :: Position -> State -> Either Error ()
guardNotOccupied pos state =
  LocationAlreadyOccupied `thrownWhen` (occupied pos state)

guardTurn :: Stone -> State -> Either Error ()
guardTurn stone state =
  OutOfTurn `thrownWhen` (turn state /= stone)

guardInBoardBoundaries :: Position -> State -> Either Error ()
guardInBoardBoundaries Pos{x, y} state =
  OutOfBounds `thrownWhen` (not (0 <= x && x < size state && 0 <= y && y < size state))

guardPieceWouldHaveLiberties :: Position -> State -> Either Error ()
guardPieceWouldHaveLiberties pos state =
  PlacementHasNoLiberties `thrownWhen` wouldNotHaveLiberties pos state

execute' :: Command -> State -> Either Error Event
execute' (Place s p) state = do
  guardNotOccupied p state
  guardTurn s state
  guardInBoardBoundaries p state
  guardPieceWouldHaveLiberties  p state
  pure $ StonePlaced s p
execute' Pass _ =
  pure TurnPassed
execute' Resign _ =
  pure PlayerResigned
