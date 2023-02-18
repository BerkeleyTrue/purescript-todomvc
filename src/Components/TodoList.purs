module Components.TodoList (todoList) where

import Components.TodoItem (todoItem)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

todoList :: forall props act. HH.HTML props act
todoList = HH.ul [ HP.classes [ HH.ClassName "todo-list" ] ]
  [ todoItem
  ]
