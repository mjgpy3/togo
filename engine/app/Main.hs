module Main where

import Commands (execute, Command(..))
import Core (emptyGame, turn, State, widthAndHeight, Event)
import Render (render)
import System.Console.ANSI (clearScreen)
import Text.Read (readMaybe)

inBoardBoundaries :: (Int, Int) -> State -> Bool
inBoardBoundaries (x, y) state =
  let (width, height) = widthAndHeight state
  in 1 <= x && x <= width && 1 <= y && y <= height

parseCommand :: State -> IO Command
parseCommand state = do
  line <- getLine
  let xText = takeWhile (/= ' ') line
  let yText = dropWhile (/= ' ') line
  case (readMaybe xText, readMaybe yText) of
    (Just x, Just y) ->
      if inBoardBoundaries (x, y) state
      then pure (Place (turn state) (x-1, y-1))
      else do
        putStrLn (line ++ " are not within the expected boundaries " ++ show (widthAndHeight state))
        parseCommand state
    _ -> do
      putStrLn $ "Expected two numbers but got " ++ line
      parseCommand state

game :: State -> [Event] -> IO ()
game state events = do
  clearScreen
  putStrLn (show (turn state) ++ "'s turn")
  putStrLn $ render state
  command <- parseCommand state
  case execute command state of
    Left _ -> do
      putStrLn "An unexpected error occured while running command"
      game state events
    Right (newState, newEvents) ->
      game newState (newEvents ++ events)

main :: IO ()
main = game emptyGame []
