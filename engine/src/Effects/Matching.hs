{-# LANGUAGE NamedFieldPuns #-}

module Effects.Matching
  (
    Matching(..)
  , runSingleMatchInState
  , createMatch
  , saveEvent
  , getEvents
  ) where

import Core(Event(..), Stone(..), Position(..))
import qualified Match as M
import Polysemy
import Polysemy.State
import Data.GUID
import System.Directory
import Text.JSON
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

data Matching m a where
  CreateMatch :: Matching m M.Match
  SaveEvent :: Event -> M.Match -> Matching m ()
  GetEvents :: M.Match -> Matching m [Event]
makeSem ''Matching

runSingleMatchInState :: Member (State [Event]) r => Sem (Matching ': r) a -> Sem r a
runSingleMatchInState = interpret $ \case
  CreateMatch -> pure $ M.identifiedBy "fake"
  SaveEvent event _ -> modify ((:) event)
  GetEvents _ -> get

{-
  = StonePlaced Stone Position
  | TurnPassed
  | PlayerResigned
 - -}

parseStone' "Black" = Ok Black
parseStone' "White" = Ok White
parseStone' _ = Error "Unrecognized stone"

instance JSON Position where
  readJSON (JSObject o) = Pos <$> valFromObj "x" o <*> valFromObj "y" o

  readJSON _ = Error "Expected JS Object"

  showJSON Pos{x, y} = JSObject $ toJSObject [("x", showJSON x), ("y", showJSON y)]

instance JSON Event where
  readJSON (JSObject o) = do
    t <- valFromObj "type" o
    case (t, valFromObj "stone" o, valFromObj "position" o) of
      ("TurnPassed", _, _) -> Ok TurnPassed
      ("PlayerResigned", _, _) -> Ok PlayerResigned
      ("StonePlaced", Ok stone, Ok position) -> do
        st <- parseStone' stone
        Ok $ StonePlaced st position
      _ -> Error "Could not parse event"

  readJSON _ = Error "Expected JS Object"

  showJSON TurnPassed = JSObject $ toJSObject [("type", showJSON "TurnPassed")]
  showJSON PlayerResigned = JSObject $ toJSObject [("type", showJSON "PlayerResigned")]
  showJSON (StonePlaced stone pos) =
    JSObject $ toJSObject [ ("type", showJSON "StonePlaced")
                          , ("stone", showJSON (show stone))
                          , ("position", showJSON pos)
                          ]

runMatchWithFileSystemStore :: Member (Lift IO) r => Sem (Matching ': r) a -> Sem r a
runMatchWithFileSystemStore = interpret $ \case
  CreateMatch -> sendM createMatchAndDir
  SaveEvent event match -> sendM $ saveEventInFile event match
  GetEvents match -> sendM $ readEventsFromFiles match

  where
    matchDir :: M.Match -> String
    matchDir match = "/tmp/" ++ M.identifier match ++ "/"

    createMatchAndDir :: IO M.Match
    createMatchAndDir = do
      guid <- genString
      let match = M.identifiedBy guid
      createDirectory (matchDir match)
      pure $ match

    saveEventInFile :: Event -> M.Match -> IO ()
    saveEventInFile event match = do
      fileCount <- length <$> listDirectory (matchDir match)
      writeFile (matchDir match ++ show fileCount) (encode event)

    readEventsFromFiles :: M.Match -> IO [Event]
    readEventsFromFiles match = do
      (fileNumbers :: [Int]) <- catMaybes <$> map readMaybe <$> listDirectory (matchDir match)
      let sorted = reverse $ sort fileNumbers
      sortedEventText <- mapM (readFile . show) sorted
      case traverse decode sortedEventText of
        Ok events -> pure events
        _ -> pure []
