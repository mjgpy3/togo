module Main where

import Commands (execute, Command(..), Error(..))
import Core (emptyGame, turn, State, widthAndHeight, Event)
import Effects.Tty
import Polysemy
import Render (render)
import Text.Read (readMaybe)

inBoardBoundaries :: (Int, Int) -> State -> Bool
inBoardBoundaries (x, y) state =
  let (width, height) = widthAndHeight state
  in 1 <= x && x <= width && 1 <= y && y <= height

parseCommand :: Member Tty r => State -> Sem r Command
parseCommand state = do
  line <- readTty
  let xText = takeWhile (/= ' ') line
  let yText = dropWhile (/= ' ') line
  case (readMaybe xText, readMaybe yText) of
    (Just x, Just y) ->
      if inBoardBoundaries (x, y) state
      then pure (Place (turn state) (x-1, y-1))
      else do
        writeTty (line ++ " are not within the expected boundaries " ++ show (widthAndHeight state))
        parseCommand state
    _ -> do
      writeTty $ "Expected two numbers but got " ++ line
      parseCommand state

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken!"
formatError OutOfTurn = "It's not your turn!"


game :: Member Tty r => State -> [Event] -> Sem r ()
game state events = do
  writeTty (show (turn state) ++ "'s turn")
  writeTty $ render state
  command <- parseCommand state
  clearTty
  case execute command state of
    Left e -> do
      writeTty ("Error: " ++ formatError e)
      game state events
    Right (newState, newEvents) -> do
      game newState (newEvents ++ events)

gameIO :: State -> [Event] -> Sem '[Lift IO] ()
gameIO = (.) runTtyIo . game

main :: IO ()
main = runM $ gameIO emptyGame []
