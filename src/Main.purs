module Main where

import Components.App (app)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI app unit body
