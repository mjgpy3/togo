{-# LANGUAGE NamedFieldPuns #-}

module Effects.Matching
  (
    Matching(..)
  , runSingleMatchInState
  , runMatchWithFileSystemStore
  , createMatch
  , saveEvent
  , getEvents
  ) where

import Control.Monad (unless)
import Core(Event(..), Stone(..), Position(..))
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
makeSem ''Matching

runSingleMatchInState :: Member (State [Event]) r => Sem (Matching ': r) a -> Sem r a
runSingleMatchInState = interpret $ \case
  CreateMatch -> pure $ M.identifiedBy "fake"
  SaveEvent event _ -> modify ((:) event)
  GetEvents _ -> get

runMatchWithFileSystemStore :: Member (Lift IO) r => Sem (Matching ': r) a -> Sem r a
runMatchWithFileSystemStore = interpret $ \case
  CreateMatch -> sendM createMatchAndDir
  SaveEvent event match -> sendM $ saveEventInFile event match
  GetEvents match -> sendM $ readEventsFromFiles match

  where
    matchDir :: M.Match -> String
    matchDir match = "/tmp/togo/" ++ M.identifier match ++ "/"

    createMatchAndDir :: IO M.Match
    createMatchAndDir = do
      guid <- genString
      let match = M.identifiedBy guid
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
