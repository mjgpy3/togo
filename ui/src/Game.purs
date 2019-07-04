module Game where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Data.Either (hush, Either(Left))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Halogen.HTML.CSS (style)
import CSS as CSS
import Data.Array as Array

newtype Position
  = Pos { x :: Int
        , y :: Int
        }

instance eqPosition :: Eq Position where
  eq (Pos a) (Pos b) = a.x == b.x && a.y == b.y

data Stone
  = Black
  | White

instance showStone :: Show Stone where
  show Black = "Black"
  show White = "White"

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

type Match = { matchId :: MatchIdentifier
             , game :: Game
             , currentHighlight :: Maybe Position
             }

data State a
  = NoneYet
  | Loading
  | UnexpectedError
  | LocalMatch a

data MatchIdentifier = MatchIdentifier String

instance functorGameState :: Functor State where
  map _ NoneYet = NoneYet
  map _ Loading = Loading
  map _ UnexpectedError = UnexpectedError
  map f (LocalMatch v) = LocalMatch $ f v

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
  | HighlightStone Position
  | ClearHighlightedStone
  | PlaceStone Stone MatchIdentifier Position

component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State Match
initialState _ = NoneYet

render :: forall m. State Match -> H.ComponentHTML Action () m
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
render (LocalMatch m) = renderGame m

renderGame :: forall m. Match -> H.ComponentHTML Action () m
renderGame { game: Game g, currentHighlight, matchId } =
  HH.table [ style do CSS.key (CSS.fromString "border-spacing") (CSS.px 0.0)
                      CSS.border CSS.solid (CSS.px 2.0) (CSS.graytone 0.2) 
                      CSS.backgroundColor (fromMaybe (CSS.graytone 0.5) $ CSS.fromHexString "#966F33")
           , HE.onMouseOut (Just <<< const ClearHighlightedStone) ] $ do
    y <- Array.range 0 g.height
    pure $ HH.tr_ $ do
      x <- Array.range 0 g.width
      pure $ HH.td [ style do CSS.border CSS.solid (CSS.px 0.5) CSS.black
                              CSS.height (CSS.rem 2.5)
                              CSS.width (CSS.rem 2.5) ]
        [ HH.table [ style do CSS.key (CSS.fromString "border-spacing") (CSS.px 0.0)
                              CSS.height (CSS.pct 100.0)
                              CSS.width (CSS.pct 100.0)
                   ]
            (
              let
                highlightable x' y' = [ HE.onMouseOver (Just <<< const (HighlightStone $ Pos { x: x', y: y' }))
                                      , HE.onClick (Just <<< const (PlaceStone g.turn matchId $ Pos { x: x', y: y' }))
                                      ]

                withinBounds x' y' =
                  0 <= x' && x' < g.width && 0 <= y' && y' < g.height

                highlighted x' y' =
                  Just (Pos { x: x', y: y' }) == currentHighlight

                highlight = [ style do CSS.backgroundColor (fromMaybe (CSS.graytone 0.5) $ CSS.fromHexString "#FF0") ]

                chunkAt x' y' =
                  HH.td ((if withinBounds x' y' then highlightable x' y' else []) <> (if highlighted x' y' then highlight else [])) []
              in
                [ HH.tr_ [ chunkAt (x - 1) (y - 1)
                         , chunkAt x (y - 1)
                         ]
                , HH.tr_ [ chunkAt (x - 1) y
                         , chunkAt x y
                         ]
                ]
            )
        ]


noContent :: AXRB.RequestBody
noContent = AXRB.string ""

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM (State Match) Action () o m Unit
handleAction = case _ of
  PlaceStone stone (MatchIdentifier matchId) (Pos p) -> do
     gameResponse <- H.liftAff $ AX.post AXRF.json ("/api/match/" <> matchId <> "/" <> show stone <> "/place/" <> show p.x <> "/" <> show p.y) noContent
     decodeAndRenderGame matchId gameResponse.body
  ClearHighlightedStone ->
    H.modify_ $ map (\m -> m { currentHighlight = Nothing })
  HighlightStone pos ->
    H.modify_ $ map (\m -> m { currentHighlight = Just pos })
  StartNewLocalMatch event -> do
    H.liftEffect $ Event.preventDefault event
    H.put Loading
    createMatchResponse <- H.liftAff $ AX.post AXRF.json "/api/match" noContent
    case justDecode createMatchResponse.body of
      Nothing -> H.put UnexpectedError
      Just (MatchIdentifier matchId) -> loadGameByMatchId matchId

  where
    justDecode :: forall v e. DecodeJson v => Either e A.Json -> Maybe v
    justDecode = hush >=> (hush <<< decodeJson)

    loadGameByMatchId :: forall o' m'. MonadAff m' => String -> H.HalogenM (State Match) Action () o' m' Unit
    loadGameByMatchId matchId = do
      gameResponse <- H.liftAff $ AX.get AXRF.json ("/api/match/" <> matchId <> "/game")
      decodeAndRenderGame matchId gameResponse.body

    decodeAndRenderGame :: forall o' m' e. MonadAff m' => String -> Either e A.Json -> H.HalogenM (State Match) Action () o' m' Unit
    decodeAndRenderGame matchId gameBody = do
      case justDecode gameBody of
        Nothing -> H.put UnexpectedError
        Just game ->
          H.put $ LocalMatch { matchId: MatchIdentifier matchId, currentHighlight: Nothing, game }
