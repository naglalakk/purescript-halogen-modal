module Example.Component where

import Prelude
import CSS                                  as CSS
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..))
import Data.Symbol                          (SProxy(..))
import Effect.Class                         (class MonadEffect)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Halogen.HTML.CSS                     as HCSS

import Halogen.Modal                        as Modal
import Halogen.Modal.Utils                  (css)

type Input = Unit

type State =
  { modalActive :: Boolean
  }

type Query = Const Void

data Action = ModalTrigger

type ChildSlots = (
  modal :: H.Slot Query Void Unit
)

initialState :: State
initialState =
  { modalActive: false
  }

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Void m
component = 
  H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }
  where
  handleAction = case _ of
    ModalTrigger -> H.modify_ _ { modalActive = true }

  render :: State -> H.ComponentHTML Action ChildSlots m 
  render state = 
    HH.div
      [ HCSS.style do
        CSS.display CSS.flex
        CSS.justifyContent $ CSS.JustifyContentValue 
                           $ CSS.Value 
                           $ CSS.Plain "center"
        CSS.alignItems $ CSS.AlignItemsValue 
                       $ CSS.Value 
                       $ CSS.Plain "center"
      , css "modal-example"
      ]
      [ HH.button
        [ HE.onClick \_ -> Just ModalTrigger 
        ]
        [ HH.text "Click me" ]
      , HH.slot (SProxy :: _ "modal")
        unit
        (Modal.component)
        { isActive: state.modalActive 
        , html: (HH.p [] [HH.text "wow what a nice modal you got there"])
        }
        absurd
      ]
