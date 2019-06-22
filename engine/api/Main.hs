module Main where

import Commands (execute, Command(..), Error(..))
import Control.Monad (msum, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Data.Aeson
import Data.Char (toLower)
import Effects.Tty
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, dir, method, Method(GET), ServerPartT)
import Happstack.Server.Response (ToMessage)
import Polysemy
import Render (renderWithColRow)
import Serialization
import Text.Read (readMaybe)
import qualified Core as C

parseCommand :: Member Tty r => C.State -> Sem r Command
parseCommand state = do
  line <- readTty
  let xText = takeWhile (/= ' ') line
  let yText = dropWhile (/= ' ') line
  case (map toLower xText == "pass", readMaybe xText, readMaybe yText) of
    (True, _, _) ->
      pure Pass
    (_, Just x, Just y) ->
      pure (Place (C.turn state) (C.baseOnePosition x y))
    _ -> do
      writeTty $ "Expected two numbers or \"pass\" but got " ++ line
      parseCommand state

formatError :: Error -> String
formatError LocationAlreadyOccupied = "That position is already taken!"
formatError OutOfTurn = "It's not your turn!"
formatError OutOfBounds = "That move is not within the bounds of the board!"

game :: Member Tty r => C.State -> [C.Event] -> Sem r ()
game state events = do
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
        game state events
      Right (newState, newEvents) ->
        game newState (newEvents ++ events)

gameIO :: C.State -> [C.Event] -> Sem '[Lift IO] ()
gameIO = (.) runTtyIo . game

cliRun = runM $ gameIO C.emptyGame []

instance ToMessage C.State where

type App = StateT C.State (ServerPartT IO)

runApp :: (ToMessage a) => App a -> C.State -> IO ()
runApp app state = simpleHTTP nullConf $ fst <$> runStateT app state

singleBoardGame = do
  state <- get
  dir "api" $ msum [ getBoard state
                   , silly
                   ]
  where
    getBoard state = do
      method GET
      dir "board" $ ok $ encode $ toJSON state

    silly = do
      method GET
      dir "silly" $ lift (putStrLn "hi") >> ok (encode $ toJSON (42 :: Int))

main :: IO ()
main = runApp singleBoardGame C.emptyGame
