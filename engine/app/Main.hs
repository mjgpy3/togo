module Main where

import Commands (execute, Command(..), Error(..))
import Core (emptyGame, turn, State, widthAndHeight, Event, isEndGame)
import Data.Char (toLower)
import Effects.Tty
import Polysemy
import Render (renderWithColRow)
import Text.Read (readMaybe)

parseCommand :: Member Tty r => State -> Sem r Command
parseCommand state = do
  line <- readTty
  let xText = takeWhile (/= ' ') line
  let yText = dropWhile (/= ' ') line
  case (map toLower xText == "pass", map toLower xText == "resign", readMaybe xText, readMaybe yText) of
    (True, _, _, _) ->
      pure Pass
    (_, True, _, _) ->
      pure Resign
    (_, _, Just x, Just y) ->
      pure (Place (turn state) (x-1, y-1))
    _ -> do
      writeTty $ "Expected two numbers or \"pass\" but got " ++ line
      parseCommand state

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken!"
formatError OutOfTurn = "It's not your turn!"
formatError OutOfBounds = "That move is not within the bounds of the board!"
formatError GameEnded = "The game is over, no more moves can be made."

game :: Member Tty r => State -> [Event] -> Sem r ()
game state events = do
  writeTty (show (turn state) ++ "'s turn")
  writeTty $ renderWithColRow state
  if isEndGame state
  then
    writeTty "End of game!"
  else do
    command <- parseCommand state
    clearTty
    case execute command state of
      Left e -> do
        writeTty ("Error: " ++ formatError e)
        game state events
      Right (newState, event) ->
        game newState (event:events)

gameIO :: State -> [Event] -> Sem '[Lift IO] ()
gameIO = (.) runTtyIo . game

main :: IO ()
main = runM $ gameIO emptyGame []
