module Main where

import Commands (execute, Command(..), Error(..), formatError)
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
      pure $ Pass $ C.turn state
    (_, True, _, _) ->
      pure $ Resign $ C.turn state
    (_, _, Just x', Just y') ->
      pure (Place (C.turn state) (C.Pos {C.x=x'-1, C.y=y'-1}))
    _ -> do
      writeTty $ "Expected two numbers, \"pass\" or \"resign\" but got " ++ line
      parseCommand state

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
      writeTty $ renderCapturesBy C.Black state
      writeTty $ renderCapturesBy C.White state
      writeTty $ renderWithColRow state
      if C.isEndGame state
      then
        writeTty "End of game!"
      else do
        command <- parseCommand state
        clearTty
        case execute command events of
          Left e -> do
            writeTty ("Error: " ++ formatError e)
            play match
          Right event -> do
            saveEvent event match
            play match

renderCapturesBy :: C.Stone -> C.State -> String
renderCapturesBy stone state = "Stones captured by " ++ show stone ++ ": " ++ show (C.stonesCapturedBy stone state)

singleGameInMemoryWithIo :: Sem '[State [C.Event], Lift IO] ()
singleGameInMemoryWithIo = runTtyIo $ runSingleMatchInState game

runSingleGameInMemory :: IO ()
runSingleGameInMemory = fmap snd $ runM $ runState [] singleGameInMemoryWithIo

singleGameInFileSystem :: Sem '[Lift IO] ()
singleGameInFileSystem = runTtyIo $ runMatchWithFileSystemStore game

runSingleGameInFileSystem :: IO ()
runSingleGameInFileSystem = runM singleGameInFileSystem

main :: IO ()
main = runSingleGameInFileSystem
