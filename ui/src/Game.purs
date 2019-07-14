module Game where

import Prelude

import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXRF
import CSS as CSS
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Array as Array
import Data.Either (hush, Either(Left))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Data.Int (toNumber)
import Data.Unfoldable (replicate)

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

instance eqStone :: Eq Stone where
  eq Black Black = true
  eq White White = true
  eq _ _ = false

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

renderStone :: forall m. Stone -> H.ComponentHTML Action () m
renderStone stone =
  HH.span [ style do CSS.display CSS.grid
                     CSS.backgroundColor (if stone == White then CSS.white else CSS.black)
                     CSS.height (CSS.rem 2.5)
                     CSS.width (CSS.rem 2.5)
                     CSS.borderRadius (CSS.pct 50.0) (CSS.pct 50.0) (CSS.pct 50.0) (CSS.pct 50.0)
          ]
    []

renderGame :: forall m. Match -> H.ComponentHTML Action () m
renderGame { game: Game g, currentHighlight, matchId } =
  HH.div [ containerStyle
         , HE.onMouseOut (Just <<< const ClearHighlightedStone)
         ]
    [ placements
    , grid
    ]
  where
    containerStyle =
      style do CSS.backgroundColor wood
               CSS.position CSS.absolute
               CSS.zIndex (-1)
               CSS.width (CSS.rem (2.737 * toNumber g.width))
               CSS.height (CSS.rem (2.736 * toNumber g.height))

    placements =
      HH.table
        [ style do CSS.backgroundColor (CSS.rgba 0 0 255 0.0)
                   CSS.position CSS.absolute
                   CSS.key (CSS.fromString "border-spacing") (CSS.px 0.0)
        ]
        $ do
          y <- Array.range 0 (g.height - 1)
          pure $ HH.tr_ $ do
            x <- Array.range 0 (g.width - 1)
            pure $ HH.td ([ highlightSize ] <> cellHighlighting x y)
              (  (if occupiedByWhite x y then [ renderStone White ] else [])
                 <> (if occupiedByBlack x y then [ renderStone Black ] else [])
              )

    highlightSize = style do CSS.height (CSS.rem 2.65)
                             CSS.width (CSS.rem 2.6)

    occupied x y = occupiedByWhite x y || occupiedByBlack x y

    occupiedByWhite x y = Pos {x, y} `elem` g.whitePositions
    occupiedByBlack x y = Pos {x, y} `elem` g.blackPositions

    cellHighlighting x y =
      (if not (occupied x y) then openSlotEvents x y else [])
      <> (if highlighted x y then [highlight] else [])

    highlighted x y =
      Just (Pos { x, y }) == currentHighlight

    highlight = style do CSS.border CSS.solid (CSS.px 1.0) CSS.yellow

    openSlotEvents x y =
      [ HE.onMouseOver (Just <<< const (HighlightStone $ Pos { x: x, y: y }))
      , HE.onClick (Just <<< const (PlaceStone g.turn matchId $ Pos { x: x, y: y }))
      ]

    grid =
      HH.table
        [ style do CSS.zIndex (-1)
                   CSS.position CSS.relative
                   CSS.left (CSS.rem 1.25)
                   CSS.top (CSS.rem 1.25)
                   singlePxBlackBorder
                   CSS.border CSS.solid (CSS.px 1.0) CSS.black
                   CSS.key (CSS.fromString "border-spacing") (CSS.px 0.0)
        ]
        $ replicate (g.height - 1) $ HH.tr_ $ replicate (g.width - 1) (HH.td [ basicCellSize ] [])

    basicCellSize = style do CSS.height (CSS.rem 2.5)
                             CSS.width (CSS.rem 2.5)
                             singlePxBlackBorder

    singlePxBlackBorder = CSS.border CSS.solid (CSS.px 1.0) CSS.black


wood :: CSS.Color
wood = fromMaybe (CSS.graytone 0.5) $ CSS.fromHexString "#966F33"

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
