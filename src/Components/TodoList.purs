module Components.TodoList (todoList) where

import Prelude

import Components.TodoItem (todoItem)
import Data.Array as Arr
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (all, filter, length)
import Data.Map (Map, lookup, values)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen (Component, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useMemo, useState)
import Halogen.Hooks as Hooks
import Store (Action(..), Filter(..), Todo, useStore)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (code, toEvent)

_todoItem = Proxy :: Proxy "todoItem"

isActiveTodo :: Map Int Todo -> Int -> Boolean
isActiveTodo byId id = case lookup id byId of
  Just todo -> not todo.completed
  Nothing -> false

isCompletedTodo :: Map Int Todo -> Int -> Boolean
isCompletedTodo byId id = case lookup id byId of
  Just todo -> todo.completed
  Nothing -> false

todoList :: forall query input output m. MonadEffect m => Component query input output m
todoList = Hooks.component \_ _ -> Hooks.do
  newVal /\ newValId <- useState ""
  filter' /\ ctx <- useStore \s -> s.filter
  (todos' /\ byId) /\ _ <- useStore \s -> (s.todos /\ s.todosById)

  let
    todos = case filter' of
      All -> todos'
      Active -> Arr.filter (isActiveTodo byId) todos'
      Completed -> Arr.filter (isCompletedTodo byId) todos'

  isAllSelected <- useIsAllSelected filter' byId

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

    handleToggleAll = \_ -> do
      log $ "Toggling all todos: " <> show isAllSelected
      ctx.dispatch $ ToggleTodos $ not isAllSelected

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
              , HP.id "toggle-all"
              , HP.type_ HP.InputCheckbox
              , HP.checked isAllSelected
              , HE.onClick handleToggleAll
              ]
          , HH.label [ HP.for "toggle-all" ] []
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
                      , HP.class_ (HH.ClassName if filter' == All then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter All
                      ]
                      [ HH.text "All" ]
                  ]
              , HH.li []
                  [ HH.a
                      [ HP.href "#/active"
                      , HP.class_ (HH.ClassName if filter' == Active then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter Active
                      ]
                      [ HH.text "Active" ]
                  ]
              , HH.li []
                  [ HH.a
                      [ HP.href "#/completed"
                      , HP.class_ (HH.ClassName if filter' == Completed then "selected" else "")
                      , HE.onClick \_ -> ctx.dispatch $ SetFilter Completed
                      ]
                      [ HH.text "Completed" ]
                  ]
              ]
          , HH.button [ HP.classes [ HH.ClassName "clear-completed" ] ]
              [ HH.text "Clear completed" ]
          ]
      ]
  where
  useIsAllSelected filter' byId =
    Hooks.captures { filter', byId }
      $ flip useMemo (\_ -> all (\todo -> todo.completed)
      $ values byId)
