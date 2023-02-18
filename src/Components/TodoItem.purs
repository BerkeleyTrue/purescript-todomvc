module Components.TodoItem
  ( todoItem
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

todoItem :: forall props act. HH.HTML props act
todoItem = HH.li [ HP.classes [ HH.ClassName "todo" ] ]
  [ HH.div [ HP.classes [ HH.ClassName "view" ] ]
      [ HH.input [ HP.classes [ HH.ClassName "toggle" ], HP.type_ HP.InputCheckbox ]
      , HH.label [] [ HH.text "some todo" ]
      , HH.button [ HP.classes [ HH.ClassName "destroy" ] ] []
      ]
  ]
