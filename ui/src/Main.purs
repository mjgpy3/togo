module Main where

import Prelude

import Effect (Effect)
import Game as Game
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI Game.component unit body
