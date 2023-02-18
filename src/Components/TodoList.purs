module Components.TodoList (todoList) where

import Components.TodoItem (todoItem)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

todoList :: forall props act. HH.HTML props act
todoList = HH.div [ HP.classes [ HH.ClassName "todoapp" ] ]
  [ HH.header [ HP.classes [ HH.ClassName "header" ] ]
      [ HH.h1 [] [ HH.text "todos" ] ]

  , HH.section
      [ HP.classes [ HH.ClassName "main" ] ]
      [ HH.input
          [ HP.classes [ HH.ClassName "toggle-all" ]
          , HP.type_ HP.InputCheckbox
          , HP.checked true
          ]
      , HH.ul [ HP.classes [ HH.ClassName "todo-list" ] ]
          [ todoItem ]
      ]

  , HH.footer [ HP.classes [ HH.ClassName "footer" ] ]
      [ HH.span [ HP.classes [ HH.ClassName "todo-count" ] ]
          [ HH.strong [] [ HH.text "0" ]
          , HH.text " items left"
          ]
      , HH.ul [ HP.classes [ HH.ClassName "filters" ] ]
          [ HH.li []
              [ HH.a [ HP.href "#/" ] [ HH.text "All" ] ]
          , HH.li []
              [ HH.a [ HP.href "#/active" ] [ HH.text "Active" ] ]
          , HH.li []
              [ HH.a [ HP.href "#/completed" ] [ HH.text "Completed" ] ]
          ]
      , HH.button [ HP.classes [ HH.ClassName "clear-completed" ] ]
          [ HH.text "Clear completed" ]
      ]
  ]
