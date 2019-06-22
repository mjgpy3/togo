module Effects.Matching
  (
    Matching(..)
  , runSingleMatchInState
  , createMatch
  , saveEvent
  , getEvents
  ) where

import Core(Event(..))
import Match
import Polysemy
import Polysemy.State

data Matching m a where
  CreateMatch :: Matching m Match
  SaveEvent :: Event -> Match -> Matching m ()
  GetEvents :: Match -> Matching m [Event]
makeSem ''Matching

runSingleMatchInState :: Member (State [Event]) r => Sem (Matching ': r) a -> Sem r a
runSingleMatchInState = interpret $ \case
  CreateMatch -> pure $ Match (Guid "fake")
  SaveEvent event _ -> modify ((:) event)
  GetEvents _ -> get
