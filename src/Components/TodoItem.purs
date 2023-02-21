module Components.TodoItem
  ( todoItem
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Store (Action(..), useStore)
import Web.Event.Event (preventDefault)
import Web.UIEvent.KeyboardEvent (code, toEvent)

type TodoInput = { todoId :: Int }

todoItem :: forall query output m. MonadEffect m => Component query TodoInput output m
todoItem = Hooks.component \_ input -> Hooks.do
  isEditing /\ isEditingId <- Hooks.useState false
  let todoId = input.todoId
  maybeTodo /\ ctx <- useStore \state -> lookup todoId state.todosById -- sometimes state is undefined?

  let
    { title, completed } = case maybeTodo of
      Just todo -> todo
      Nothing -> { id: todoId, title: "todo not found", completed: false }
    handleCheckbox = \_ -> ctx.dispatch $ ToggleTodo todoId
    handleClickOnDestroy = \_ -> ctx.dispatch $ DeleteTodo todoId
    handleViewClick = \_ -> Hooks.modify_ isEditingId $ const true
    handleOnChange = \val -> ctx.dispatch $ UpdateTodo todoId val
    finishEditing = \_ -> do
      Hooks.modify_ isEditingId $ const false
      ctx.dispatch $ UpdateTodo todoId $ trim title
    handleOnEnter = \ev -> case code ev of
      "Enter" -> do
        H.liftEffect $ preventDefault (toEvent ev)
        finishEditing unit
      _ -> pure unit

  Hooks.pure do
    HH.li
      [ HP.classes $
          join
            [ pure $ HH.ClassName "todo"
            , guard isEditing $> HH.ClassName "editing"
            , guard completed $> HH.ClassName "completed"
            ]
      , HE.onClick handleViewClick
      ]
      [ HH.div [ HP.classes [ HH.ClassName "view" ] ]
          [ HH.input
              [ HP.classes [ HH.ClassName "toggle" ]
              , HP.type_ HP.InputCheckbox
              , HP.checked completed
              , HE.onClick handleCheckbox
              , HP.autofocus true
              ]
          , HH.label [] [ HH.text title ]
          , HH.button
              [ HP.classes [ HH.ClassName "destroy" ]
              , HE.onClick handleClickOnDestroy
              ]
              []
          ]
      , if isEditing then
          HH.input
            [ HP.classes [ HH.ClassName "edit" ]
            , HP.value title
            , HE.onValueInput handleOnChange
            , HE.onKeyDown handleOnEnter
            , HP.autofocus true
            ]
        else
          HH.text ""
      ]
