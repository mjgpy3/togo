module Start where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = PlayNewMatch a

type MatchId = String

data State
  = NoneYet
  | EntireAppLoading

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = NoneYet

  render :: State -> H.ComponentHTML Query
  render EntireAppLoading =
    HH.div_
      [ HH.h1_ [HH.text "Loading..."] ]
  render NoneYet =
    HH.div_
      [ HH.h1_
          [ HH.text "Welcome to ", HH.i_ [HH.text "togo"], HH.text "!" ]
      , HH.p_
          [ HH.text "At the moment we only have one game mode, but we're working on more." ]
      , HH.button
          [ HE.onClick (HE.input_ PlayNewMatch) ]
          [ HH.text "Play new local match"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    PlayNewMatch next -> do
      _ <- H.modify (const EntireAppLoading)
      pure next
