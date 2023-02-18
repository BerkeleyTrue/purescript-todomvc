module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

component = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction _ = pure unit
  render _ = HH.div [ HP.classes [ HH.ClassName "todoapp" ] ]
    [ HH.header [ HP.classes [ HH.ClassName "header" ] ]
        [ HH.h1 [] [ HH.text "Hello, World!" ]
        ]
    ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI component unit body
