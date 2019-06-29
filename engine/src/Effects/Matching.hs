{-# LANGUAGE NamedFieldPuns #-}

module Effects.Matching
  ( Matching(..)
  , runSingleMatchInState
  , runMatchWithFileSystemStore
  , createMatch
  , saveEvent
  , getEvents
  , getMatch
  ) where

import Control.Monad (unless)
import Core(Event(..))
import qualified Match as M
import Polysemy
import Polysemy.State
import Data.GUID
import Data.Aeson
import System.Directory
import Data.List (sortOn)
import qualified Data.Ord as Ord
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Matching m a where
  CreateMatch :: Matching m M.Match
  SaveEvent :: Event -> M.Match -> Matching m ()
  GetEvents :: M.Match -> Matching m [Event]
  GetMatch :: String -> Matching m (Maybe M.Match)
makeSem ''Matching

runSingleMatchInState :: Member (State [Event]) r => Sem (Matching ': r) a -> Sem r a
runSingleMatchInState = interpret $ \case
  CreateMatch -> pure $ M.Match "fake"
  SaveEvent event _ -> modify ((:) event)
  GetEvents _ -> get
  GetMatch _ -> pure $ Just $ M.Match "fake"

runMatchWithFileSystemStore :: Member (Lift IO) r => Sem (Matching ': r) a -> Sem r a
runMatchWithFileSystemStore = interpret $ \case
  CreateMatch -> sendM createMatchAndDir
  SaveEvent event match -> sendM $ saveEventInFile event match
  GetEvents match -> sendM $ readEventsFromFiles match
  GetMatch matchId -> sendM $ toMatchIfExists matchId

  where
    matchDir :: M.Match -> String
    matchDir match = "/tmp/togo/" ++ M.identifier match ++ "/"

    toMatchIfExists :: String -> IO (Maybe M.Match)
    toMatchIfExists matchId = do
      let match = M.Match matchId
      exists <- doesDirectoryExist (matchDir match)
      pure $ if exists then Just match else Nothing

    createMatchAndDir :: IO M.Match
    createMatchAndDir = do
      guid <- genString
      let match = M.Match guid
      exists <- doesDirectoryExist "/tmp/togo"
      unless exists $ createDirectory "/tmp/togo"
      createDirectory (matchDir match)
      pure match

    saveEventInFile :: Event -> M.Match -> IO ()
    saveEventInFile event match = do
      fileCount <- length <$> listDirectory (matchDir match)
      encodeFile (matchDir match ++ show fileCount) event

    readEventsFromFiles :: M.Match -> IO [Event]
    readEventsFromFiles match = do
      (fileNumbers :: [Int]) <- mapMaybe readMaybe <$> listDirectory (matchDir match)
      let sorted = sortOn Ord.Down fileNumbers
      sortedEventText <- mapM (decodeFileStrict . (++) (matchDir match) . show) sorted
      case sequence sortedEventText of
        Just events -> pure events
        _ -> pure []
