module Start where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Data.Either (hush, Either(..))
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

data State
  = NoneYet
  | Loading
  | UnexpectedError
  | NewLocalMatch String

instance decodeJsonState :: DecodeJson State where
  decodeJson json = do
    x <- decodeJson json
    identifier <- x .: "identifier"
    pure $ NewLocalMatch identifier

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
render (NewLocalMatch id) = HH.p_ [ HH.text ("New match " <> id) ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  StartNewLocalMatch event -> do
    H.liftEffect $ Event.preventDefault event
    H.put Loading
    response <- H.liftAff $ AX.post AXRF.json "/api/match" (AXRB.string "")
    H.put $ newState $ hush response.body

  where
    newState :: Maybe A.Json -> State
    newState Nothing = UnexpectedError
    newState (Just json) =
      case decodeJson json of
        Right v -> v
        Left _ -> UnexpectedError
