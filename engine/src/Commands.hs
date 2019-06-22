{-# LANGUAGE NamedFieldPuns #-}

module Commands
  ( execute
  , Command(..)
  , Error(..)
  ) where

import Core (State, Event(..), Stone, Position(..), occupied, turn, widthAndHeight, isEndGame)

data Command
  = Place Stone Position
  | Pass
  | Resign

data Error
  = LocationAlreadyOccupied
  | OutOfTurn
  | OutOfBounds
  | GameEnded
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
  let (width, height) = widthAndHeight state
  in
    OutOfBounds `thrownWhen` (not (0 <= x && x < width && 0 <= y && y < height))

execute' :: Command -> State -> Either Error Event
execute' (Place s p) state = do
  guardNotOccupied p state
  guardTurn s state
  guardInBoardBoundaries p state
  pure $ StonePlaced s p
execute' Pass _ =
  pure TurnPassed
execute' Resign _ =
  pure PlayerResigned
