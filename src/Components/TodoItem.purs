module Components.TodoItem
  ( todoItem
  ) where

import Prelude

import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Store (useStore)

type TodoInput = { todoId :: Int }

todoItem :: forall query output m. MonadEffect m => Component query TodoInput output m
todoItem = Hooks.component \_ input -> Hooks.do
  let todoId = input.todoId
  maybeTodo /\ ctx <- useStore \state -> lookup todoId state.todosById
  let
    { title } = case maybeTodo of
      Just todo -> todo
      Nothing -> { id: todoId, title: "todo not found", completed: false }

  Hooks.pure do
    HH.li [ HP.classes [ HH.ClassName "todo" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "view" ] ]
          [ HH.input [ HP.classes [ HH.ClassName "toggle" ], HP.type_ HP.InputCheckbox ]
          , HH.label [] [ HH.text title ]
          , HH.button [ HP.classes [ HH.ClassName "destroy" ] ] []
          ]
      ]
