{-# LANGUAGE NamedFieldPuns #-}

module Commands
  ( execute
  , Command(..)
  , CommandResult
  , Error(..)
  , formatError
  ) where

import Core

data Command
  = Place Stone Position
  | Pass Stone
  | Resign Stone

data Error
  = LocationAlreadyOccupied
  | OutOfTurn
  | OutOfBounds
  | GameEnded
  | PlacementHasNoLiberties
  | Ko
  deriving (Eq, Show)

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken."
formatError OutOfTurn = "It's not your turn."
formatError OutOfBounds = "That move is not within the bounds of the board."
formatError GameEnded = "The game is over, no more moves can be made."
formatError PlacementHasNoLiberties = "This stone would have no liberties if placed there."
formatError Ko = "Placing a stone here would violate Ko (i.e. return the game to its previous state)."

type CommandResult = Either Error Event

execute :: Command -> [Event] -> CommandResult
execute command events =
  let state = summarize events
  in
    if isEndGame state
    then Left GameEnded
    else execute' command events state

thrownWhen :: Error -> Bool -> Either Error ()
thrownWhen err condition =
  if condition
  then Left err
  else pure ()

guardNotOccupied :: Position -> State -> Either Error ()
guardNotOccupied pos state =
  LocationAlreadyOccupied `thrownWhen` occupied pos state

guardTurn :: Stone -> State -> Either Error ()
guardTurn stone state =
  OutOfTurn `thrownWhen` (turn state /= stone)

guardInBoardBoundaries :: Position -> State -> Either Error ()
guardInBoardBoundaries Pos{x, y} state =
  OutOfBounds `thrownWhen` not (0 <= x && x < size state && 0 <= y && y < size state)

guardPieceWouldHaveLiberties :: Stone -> Position -> State -> Either Error ()
guardPieceWouldHaveLiberties current pos state =
  let
    withStonePlaced = placeStone pos current state
    isSafe = hasLiberties withStonePlaced pos current
    generatesLiberties = hasLiberties (collectCaptures withStonePlaced) pos current
  in
    PlacementHasNoLiberties `thrownWhen` not (isSafe || generatesLiberties)

guardKo :: [Event] -> State -> Stone -> Position -> Either Error ()
guardKo events state stone pos =
  let
    hasEvents = not $ null events
    previousBoard = board $ summarize $ drop 1 events
    nextBoard = board $ track (StonePlaced stone pos) state
  in
    Ko `thrownWhen` (hasEvents && previousBoard == nextBoard)

execute' :: Command -> [Event] -> State -> Either Error Event
execute' (Place s p) events state = do
  guardNotOccupied p state
  guardTurn s state
  guardInBoardBoundaries p state
  guardPieceWouldHaveLiberties s p state
  guardKo events state s p
  pure $ StonePlaced s p
execute' (Pass s) _ state =
  guardTurn s state >> pure TurnPassed
execute' (Resign s) _ state =
  guardTurn s state >> pure PlayerResigned
