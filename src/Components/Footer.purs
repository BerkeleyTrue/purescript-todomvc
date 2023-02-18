module Components.Footer (footer) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

footer :: forall props act. HH.HTML props act
footer = HH.footer [ HP.classes [ HH.ClassName "info" ] ]
  [ HH.p_ [ HH.text "Double-click to edit a todo" ]
  , HH.p_
      [ HH.text "created by "
      , HH.a [ HP.href "https://github.com/berkeleytrue" ] [ HH.text "berkeleytrue" ]
      ]
  , HH.p_
      [ HH.text "Part of "
      , HH.a [ HP.href "http://todomvc.com" ] [ HH.text "TodoMVC" ]
      ]
  ]
