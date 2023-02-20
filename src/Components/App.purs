module Components.App (app) where

import Prelude

import Components.Footer (footer)
import Components.TodoList (todoList)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

_todoList = Proxy :: Proxy "todoList"

app :: forall query input output m. MonadEffect m => H.Component query input output m
app = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction _ = pure unit
  render _ =
    HH.div_
      [ HH.slot_ _todoList unit todoList unit
      , footer
      ]
