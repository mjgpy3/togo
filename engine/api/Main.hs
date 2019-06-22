module Main where

import Commands (execute, Command(..), Error(..))
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Core (emptyGame, turn, State, widthAndHeight, Event, isEndGame)
import Data.Char (toLower)
import Effects.Tty
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, dir, method, Method(GET))
import Polysemy
import Render (renderWithColRow)
import Text.Read (readMaybe)

parseCommand :: Member Tty r => State -> Sem r Command
parseCommand state = do
  line <- readTty
  let xText = takeWhile (/= ' ') line
  let yText = dropWhile (/= ' ') line
  case (map toLower xText == "pass", readMaybe xText, readMaybe yText) of
    (True, _, _) ->
      pure Pass
    (_, Just x, Just y) ->
      pure (Place (turn state) (x-1, y-1))
    _ -> do
      writeTty $ "Expected two numbers or \"pass\" but got " ++ line
      parseCommand state

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken!"
formatError OutOfTurn = "It's not your turn!"
formatError OutOfBounds = "That move is not within the bounds of the board!"

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
      Right (newState, newEvents) ->
        game newState (newEvents ++ events)

gameIO :: State -> [Event] -> Sem '[Lift IO] ()
gameIO = (.) runTtyIo . game

cliRun = runM $ gameIO emptyGame []

main :: IO ()
main = simpleHTTP nullConf $ dir "api" $ msum [ do method GET
                                                   dir "board" $ liftIO (print "hi") >> ok "{}"
                                              ]
