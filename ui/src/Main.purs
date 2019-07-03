module Main where

import Prelude

import Effect (Effect)
import Start as Start
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Start.component unit body
