{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Maybe (mapMaybe, fromMaybe)
import Text.Read (readMaybe)
import Shelly (mkdir_p, shelly, fromText)
import System.Environment (lookupEnv)
import Data.Text (pack)

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
    baseDir :: IO String
    baseDir = do
      dir <- fromMaybe "/tmp/togo/" <$> lookupEnv "DB_DIR"
      putStrLn dir
      pure dir

    matchDir :: M.Match -> IO String
    matchDir match = do
      base <- baseDir
      pure $ base ++ M.identifier match ++ "/"

    toMatchIfExists :: String -> IO (Maybe M.Match)
    toMatchIfExists matchId = do
      let match = M.Match matchId
      dir <- matchDir match
      exists <- doesDirectoryExist dir
      pure $ if exists then Just match else Nothing

    createMatchAndDir :: IO M.Match
    createMatchAndDir = do
      guid <- genString
      let match = M.Match guid
      dir <- matchDir match
      shelly $ mkdir_p $ fromText $ pack dir
      pure match

    saveEventInFile :: Event -> M.Match -> IO ()
    saveEventInFile event match = do
      dir <- matchDir match
      fileCount <- length <$> listDirectory dir
      encodeFile (dir ++ show fileCount) event

    readEventsFromFiles :: M.Match -> IO [Event]
    readEventsFromFiles match = do
      dir <- matchDir match
      (fileNumbers :: [Int]) <- mapMaybe readMaybe <$> listDirectory dir
      let sorted = sortOn Ord.Down fileNumbers
      sortedEventText <- mapM (decodeFileStrict . (++) dir . show) sorted
      case sequence sortedEventText of
        Just events -> pure events
        _ -> pure []
