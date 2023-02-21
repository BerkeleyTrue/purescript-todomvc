module Components.TodoList (todoList) where

import Prelude

import Components.TodoItem (todoItem)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (filter, length)
import Data.Map (values)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (Component, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Store (Action(..), useStore)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (code, toEvent)

_todoItem = Proxy :: Proxy "todoItem"

todoList :: forall query input output m. MonadEffect m => Component query input output m
todoList = Hooks.component \_ _ -> Hooks.do
  newVal /\ newValId <- useState ""
  filter' /\ ctx <- useStore \s -> s.filter
  (todos /\ byId) /\ _ <- useStore \s -> (s.todos /\ s.todosById)
  let
    left = length $ filter (not <<< _.completed) $ values byId
    onNewValChange = \val -> Hooks.modify_ newValId (const val)
    onAddNewTodo ev = case code ev of
      "Enter" ->
        do
          liftEffect $ preventDefault $ toEvent ev
          if
            newVal /= "" then do
            log $ "Adding new todo: " <> newVal
            ctx.dispatch $ AddTodo newVal
            Hooks.modify_ newValId (const "")
          else pure unit
      _ -> pure unit

  Hooks.pure do
    HH.div [ HP.classes [ HH.ClassName "todoapp" ] ]
      [ HH.header [ HP.classes [ HH.ClassName "header" ] ]
          [ HH.h1 [] [ HH.text "todos" ]
          , HH.input
              [ HP.classes [ HH.ClassName "new-todo" ]
              , HP.placeholder "What needs to be done?"
              , HE.onValueInput onNewValChange
              , HE.onKeyDown onAddNewTodo
              , HP.value newVal
              ]
          ]

      , HH.section
          [ HP.classes [ HH.ClassName "main" ] ]
          [ HH.input
              [ HP.classes [ HH.ClassName "toggle-all" ]
              , HP.type_ HP.InputCheckbox
              , HP.checked true
              ]
          , HH.ul
              [ HP.classes [ HH.ClassName "todo-list" ] ]
              $ mapWithIndex
                  (\idx todoId -> HH.slot_ _todoItem idx todoItem { todoId: todoId })
                  todos

          ]

      , HH.footer [ HP.classes [ HH.ClassName "footer" ] ]
          [ HH.span [ HP.classes [ HH.ClassName "todo-count" ] ]
              [ HH.strong [] [ HH.text (show left) ]
              , HH.text " items left"
              ]
          , HH.ul [ HP.classes [ HH.ClassName "filters" ] ]
              [ HH.li []
                  [ HH.a
                      [ HP.href "#/"
                      , HP.class_ (HH.ClassName if filter' == "" then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter ""
                      ]
                      [ HH.text "All" ]
                  ]
              , HH.li []
                  [ HH.a
                      [ HP.href "#/active"
                      , HP.class_ (HH.ClassName if filter' == "active" then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter "active"
                      ]
                      [ HH.text "Active" ]
                  ]
              , HH.li []
                  [ HH.a
                      [ HP.href "#/completed"
                      , HP.class_ (HH.ClassName if filter' == "completed" then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter "completed"
                      ]
                      [ HH.text "Completed" ]
                  ]
              ]
          , HH.button [ HP.classes [ HH.ClassName "clear-completed" ] ]
              [ HH.text "Clear completed" ]
          ]
      ]
