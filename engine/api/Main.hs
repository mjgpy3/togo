{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant.API
import Servant
import Core as C
import Match as M
import qualified Network.Wai.Handler.Warp as Warp
import Effects.Matching
import Polysemy

type API
  = "match" :> Capture "id" String :> "game" :> Get '[JSON] C.State

server :: Server API
server = getGameAction

  where
    getGameAction :: String -> Handler C.State
    getGameAction matchId = do
      game <- liftIO $ runM $ getGame matchId
      case game of
        Just g' -> pure g'
        Nothing -> throwError $ err404 { errBody = "Could not find match" }

    getGame :: String -> Sem '[Lift IO] (Maybe C.State)
    getGame = runMatchWithFileSystemStore . getGame'

    getGame' :: (Member Matching r) => String -> Sem r (Maybe C.State)
    getGame' matchId = do
      match <- getMatch matchId
      case match of
        Nothing -> pure Nothing
        Just m -> Just . C.summarize <$> getEvents m

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = Warp.run 8081 app
