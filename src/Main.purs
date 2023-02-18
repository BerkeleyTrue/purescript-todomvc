module Main where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

todoItem :: forall props act. HH.HTML props act
todoItem = HH.li [ HP.classes [ HH.ClassName "todo" ] ]
  [ HH.div [ HP.classes [ HH.ClassName "view" ] ]
      [ HH.input [ HP.classes [ HH.ClassName "toggle" ], HP.type_ HP.InputCheckbox ]
      , HH.label [] [ HH.text "some todo" ]
      , HH.button [ HP.classes [ HH.ClassName "destroy" ] ] []
      ]
  ]

todoList :: forall props act. HH.HTML props act
todoList = HH.ul [ HP.classes [ HH.ClassName "todo-list" ] ]
  [ todoItem
  ]

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

appMain :: forall q m. MonadEffect m => H.Component q Unit Unit m
appMain = H.mkComponent
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

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI appMain unit body
