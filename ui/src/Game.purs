module Game where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Data.Either (hush, Either(Left))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))

newtype Position
  = Pos { x :: Int
        , y :: Int
        }

data Stone
  = Black
  | White

newtype Game
  = Game { whiteCaptures :: Int
         , whitePositions :: Array Position
         , blackCaptures :: Int
         , blackPositions :: Array Position
         , turn :: Stone
         , height :: Int
         , width :: Int
         , over :: Boolean
         }

data State
  = NoneYet
  | Loading
  | UnexpectedError
  | LocalMatch MatchIdentifier Game

data MatchIdentifier = MatchIdentifier String

instance decodeJsonPosition :: DecodeJson Position where
  decodeJson json = do
    obj <- decodeJson json
    x <- obj .: "x"
    y <- obj .: "y"
    pure $ Pos { x, y }

instance decodeJsonStone :: DecodeJson Stone where
  decodeJson =
    A.caseJsonString (Left "Expected a string") $ case _ of
      "Black" -> pure Black
      "White" -> pure White
      _ -> Left "Expected exactly \"Black\" or \"White\""

instance decodeJsonGame :: DecodeJson Game where
  decodeJson json = do
    obj <- decodeJson json
    white <- obj .: "white"
    whiteCaptures <- white .: "captures"
    whitePositions <- white .: "positions"
    black <- obj .: "black"
    blackCaptures <- black .: "captures"
    blackPositions <- black .: "positions"
    turn <- obj .: "turn"
    pure $ Game { whiteCaptures
                , whitePositions
                , blackCaptures
                , blackPositions
                , turn
                , height: 19
                , width: 19
                , over: false
                }

instance decodeJsonMatchIdentifier :: DecodeJson MatchIdentifier where
  decodeJson json = do
    x <- decodeJson json
    identifier <- x .: "identifier"
    pure $ MatchIdentifier identifier

data Action
  = StartNewLocalMatch Event

component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = NoneYet

render :: forall m. State -> H.ComponentHTML Action () m
render NoneYet =
  HH.form
    [ HE.onSubmit (Just <<< StartNewLocalMatch) ]
    [ HH.h1_ [ HH.text "Togo" ]
    , HH.p_
        [ HH.text "At the moment we only have one game mode..." ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit ]
        [ HH.text "Play single match" ]
    ]
render Loading = HH.p_ [ HH.text "Loading..." ]
render UnexpectedError = HH.p_ [ HH.text "An unexpected error has occured, please try again later..." ]
render (LocalMatch _ _) = HH.p_ [ HH.text "New match " ]

noContent :: AXRB.RequestBody
noContent = AXRB.string ""

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  StartNewLocalMatch event -> do
    H.liftEffect $ Event.preventDefault event
    H.put Loading
    createMatchResponse <- H.liftAff $ AX.post AXRF.json "/api/match" noContent
    case justDecode createMatchResponse.body of
      Nothing -> H.put UnexpectedError
      Just (MatchIdentifier matchId) -> do
        gameResponse <- H.liftAff $ AX.get AXRF.json ("/api/match/" <> matchId <> "/game")
        case justDecode gameResponse.body of
          Nothing -> H.put UnexpectedError
          Just game ->
            H.put $ LocalMatch (MatchIdentifier matchId) game

  where
    justDecode :: forall v e. DecodeJson v => Either e A.Json -> Maybe v
    justDecode = hush >=> (hush <<< decodeJson)
