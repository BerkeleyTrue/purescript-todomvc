module Components.App (app) where

import Components.Footer (footer)
import Components.TodoList (todoList)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Prelude

app :: forall q m. MonadEffect m => H.Component q Unit Unit m
app = H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction _ = pure unit
  render _ =
    HH.div_
      [ todoList
      , footer
      ]
