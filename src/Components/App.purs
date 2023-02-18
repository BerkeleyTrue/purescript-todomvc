module Components.App (app) where

import Components.Footer (footer)
import Components.TodoList (todoList)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude

app :: forall q m. MonadEffect m => H.Component q Unit Unit m
app = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction _ = pure unit
  render _ =
    HH.div_
      [ HH.div [ HP.classes [ HH.ClassName "todoapp" ] ]
          [ HH.header [ HP.classes [ HH.ClassName "header" ] ]
              [ HH.h1 [] [ HH.text "todos" ] ]
          ]
      , todoList
      , footer
      ]
