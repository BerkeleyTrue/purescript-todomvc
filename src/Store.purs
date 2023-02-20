module Store where

import Prelude
import Data.Array (filter, snoc)
import Data.Map (Map, singleton, union, update)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen.Helix (UseHelixHook, makeStore')

type Todo =
  { id :: Int
  , title :: String
  , completed :: Boolean
  }

type Store =
  { todos :: Array Int
  , todosById :: Map Int Todo
  , filter :: String
  , currentId :: Int
  }

data Action
  = AddTodo String
  | CompleteTodo Int
  | ToggleTodo Int
  | SetFilter String
  | DeleteTodo Int

getId :: Store -> Int
getId store = store.currentId

reducer :: Store -> Action -> Store
reducer store = case _ of
  AddTodo title ->
    store
      { todos = snoc store.todos newId
      , todosById = union store.todosById $ singleton newId { id: newId, title: title, completed: false }
      , currentId = newId
      }
    where
    newId = getId store

  DeleteTodo id ->
    store
      { todos = filter (\todo -> todo /= id) store.todos
      , todosById = update (\_ -> Nothing) id store.todosById
      }

  CompleteTodo id ->
    store
      { todosById = update (\todo -> Just todo { completed = true }) id store.todosById
      }

  ToggleTodo id ->
    store
      { todosById = update (\todo -> Just todo { completed = not todo.completed }) id store.todosById
      }

  SetFilter filter ->
    store { filter = filter }

createUseStore :: forall part m. MonadEffect m => Eq part => Store -> UseHelixHook Store Action part m
createUseStore = makeStore' "todos" reducer

initStore :: Store
initStore =
  { currentId: 1
  , todos: [ 1 ]
  , todosById: singleton 1 { id: 1, title: "Hello", completed: false }
  , filter: ""
  }

useStore :: forall part m. MonadEffect m => Eq part => UseHelixHook Store Action part m
useStore = createUseStore initStore
