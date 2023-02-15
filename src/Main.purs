module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (text)
import Halogen.VDom.Driver (runUI)

component = H.mkComponent
  { initialState: const unit
  , render: \_ -> text "Hello, World!"
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
    handleAction _ = pure unit

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body
