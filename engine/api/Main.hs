{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant.API
import Servant
import Core as C
import Match as M
import Commands
import qualified Network.Wai.Handler.Warp as Warp
import Effects.Matching
import Polysemy
import Data.ByteString.Lazy.Char8 (pack)

type API
  =    "api" :> "match" :> Capture "id" String :> "game" :> Get '[JSON] C.State
  :<|> "api" :> "match" :> Post '[JSON] M.Match
  :<|> "api" :> "match" :> Capture "id" String :> Capture "color" C.Stone :> "place" :> Capture "x" Int :> Capture "y" Int :> Post '[JSON] C.State

instance FromHttpApiData C.Stone where
  parseUrlPiece "black" = pure C.Black
  parseUrlPiece "Black" = pure C.Black
  parseUrlPiece "white" = pure C.White
  parseUrlPiece "White" = pure C.White
  parseUrlPiece "White" = pure C.White
  parseUrlPiece _ = Left "Unrecognized stone color, must be \"Black\" or \"White\""

data ApiCommandError
  = CommandError Error
  | MatchNotFound

server :: Server API
server = getGameAction :<|> makeMatchAction :<|> placeStoneAction

  where
    placeStoneAction :: String -> Stone -> Int -> Int -> Handler C.State
    placeStoneAction matchId s x y = commandAction matchId (Place s (C.Pos x y))

    commandAction :: String -> Command -> Handler C.State
    commandAction matchId c = do
      game <- liftIO $ runM $ command matchId c
      case game of
        Right g' -> pure g'
        Left MatchNotFound -> throwError $ err404 { errBody = "Could not find match" }
        Left (CommandError e) -> throwError $ err400 { errBody = pack $ formatError e }

    makeMatchAction :: Handler M.Match
    makeMatchAction = liftIO $ runM makeMatch

    getGameAction :: String -> Handler C.State
    getGameAction matchId = do
      game <- liftIO $ runM $ getGame matchId
      case game of
        Just g' -> pure g'
        Nothing -> throwError $ err404 { errBody = "Could not find match" }

    command :: String -> Command -> Sem '[Lift IO] (Either ApiCommandError C.State)
    command matchId = runMatchWithFileSystemStore . command' matchId

    command' :: Member Matching r => String -> Command -> Sem r (Either ApiCommandError C.State)
    command' matchId c = do
      match <- getMatch matchId
      case match of
        Nothing -> pure $ Left MatchNotFound
        Just m -> do
          events <- getEvents m
          let state = C.summarize events
          case execute c events of
            Left e -> pure $ Left $ CommandError e
            Right e -> pure $ Right $ track e state

    getGame :: String -> Sem '[Lift IO] (Maybe C.State)
    getGame = runMatchWithFileSystemStore . getGame'

    getGame' :: Member Matching r => String -> Sem r (Maybe C.State)
    getGame' matchId = do
      match <- getMatch matchId
      case match of
        Nothing -> pure Nothing
        Just m -> Just . C.summarize <$> getEvents m

    makeMatch :: Sem '[Lift IO] M.Match
    makeMatch = runMatchWithFileSystemStore createMatch

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = Warp.run 8081 app
