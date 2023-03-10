module Store where

import Prelude

import Data.Array (filter, snoc)
import Data.Map as Mp
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen.Helix (UseHelixHook, makeStore')

type Todo =
  { id :: Int
  , title :: String
  , completed :: Boolean
  }

data Filter
  = All
  | Active
  | Completed

derive instance eqFilter :: Eq Filter

type Store =
  { todos :: Array Int
  , todosById :: Mp.Map Int Todo
  , filter :: Filter
  , currentId :: Int
  }

data Action
  = AddTodo String
  | UpdateTodo Int String
  | CompleteTodo Int
  | ToggleTodo Int
  | ToggleTodos Boolean
  | DeleteTodo Int
  | SetFilter Filter

getId :: Store -> Int
getId store = store.currentId + 1

reducer :: Store -> Action -> Store
reducer store = case _ of
  AddTodo title ->
    store
      { todos = snoc store.todos newId
      , todosById = Mp.union store.todosById $ Mp.singleton newId newTodo
      , currentId = newId
      }
    where
    newId = getId store
    newTodo = { id: newId, title: title, completed: false }

  UpdateTodo id title ->
    store
      { todosById = Mp.update (\todo -> Just todo { title = title }) id store.todosById }

  DeleteTodo id ->
    store
      { todos = filter (\todo -> todo /= id) store.todos
      , todosById = Mp.update (\_ -> Nothing) id store.todosById
      }

  CompleteTodo id ->
    store
      { todosById = Mp.update (\todo -> Just todo { completed = true }) id store.todosById
      }

  ToggleTodo id ->
    store
      { todosById = Mp.update (\todo -> Just todo { completed = not todo.completed }) id store.todosById
      }

  ToggleTodos toState ->
    store
      { todosById = Mp.mapMaybe (\todo -> Just $ todo { completed = toState }) store.todosById
      }

  SetFilter filter ->
    store { filter = filter }

createUseStore :: forall part m. MonadEffect m => Eq part => Store -> UseHelixHook Store Action part m
createUseStore = makeStore' "todos" reducer

initStore :: Store
initStore =
  { currentId: 2
  , todos: [ 1, 2 ]
  , todosById: Mp.fromFoldable
      [ (Tuple 1 { id: 1, title: "Learn Halogen", completed: true })
      , (Tuple 2 { id: 2, title: "Learn Halogen Hooks", completed: false })
      ]
  , filter: All
  }

useStore :: forall part m. MonadEffect m => Eq part => UseHelixHook Store Action part m
useStore = createUseStore initStore
