module Main where

import Data.Proxy
import Servant.API
import Servant
import Core as C
import Network.Wai.Handler.Warp

type API
  = "match" :> Capture "id" String :> "game" :> Get '[JSON] C.State

server :: Server API
server = getGame

getGame :: String -> Handler C.State
getGame matchId = do
  undefined

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
