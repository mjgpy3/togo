module Main where

import Commands (execute, Command(..), Error(..))
import qualified Core as C
import qualified Match as M
import Data.Char (toLower)
import Effects.Tty
import Effects.Matching
import Polysemy
import Polysemy.State
import Render (renderWithColRow)
import Text.Read (readMaybe)

parseCommand :: Member Tty r => C.State -> Sem r Command
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
      pure (Place (C.turn state) (x-1, y-1))
    _ -> do
      writeTty $ "Expected two numbers or \"pass\" but got " ++ line
      parseCommand state

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken!"
formatError OutOfTurn = "It's not your turn!"
formatError OutOfBounds = "That move is not within the bounds of the board!"
formatError GameEnded = "The game is over, no more moves can be made."

game :: (Member Tty r, Member Matching r) => Sem r ()
game = do
  match <- createMatch
  play match
  where
    play :: (Member Tty r, Member Matching r) => M.Match -> Sem r ()
    play match = do
      events <- getEvents match
      let state = C.summarize events

      writeTty (show (C.turn state) ++ "'s turn")
      writeTty $ renderWithColRow state
      if C.isEndGame state
      then
        writeTty "End of game!"
      else do
        command <- parseCommand state
        clearTty
        case execute command state of
          Left e -> do
            writeTty ("Error: " ++ formatError e)
            play match
          Right event -> do
            saveEvent event match
            play match

gameIO :: Sem '[State [C.Event], Lift IO] ()
gameIO = runTtyIo $ runSingleMatchInState game

main :: IO ()
main = fmap snd $ runM $ runState [] $ gameIO
