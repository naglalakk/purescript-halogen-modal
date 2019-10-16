module Halogen.Modal where 

import Prelude
import CSS                      as CSS
import Effect.Class.Console     (logShow)
import Effect.Class             (class MonadEffect)
import Data.Const               (Const)
import Data.Maybe               (Maybe(..))
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Events      as HE
import Halogen.HTML.CSS         as HCSS
import Halogen.Modal.Utils      (css)

type Query = Const Void

type ChildSlots = ()

data Action
  = Receive Input
  | CloseModal

type Input =
  { html :: forall i p. HH.HTML i p
  , isActive :: Boolean
  }

type State =
  { html :: forall i p. HH.HTML i p
  , isActive :: Boolean
  }

initialState :: Input -> State
initialState inp = 
  { html: inp.html
  , isActive: inp.isActive
  }

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Void m
component = 
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of 
    Receive inp -> do
      H.liftEffect $ logShow inp.isActive
      H.modify_ _ { html = inp.html 
                  , isActive = inp.isActive
                  }
    CloseModal  -> do
      state <- H.get
      case state.isActive of
        false -> pure unit
        true  -> H.modify_ _ { isActive = false }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div
      [ HCSS.style do
        CSS.position CSS.absolute
        CSS.top $ CSS.px 0.0
        CSS.width $ CSS.pct 100.0
        CSS.height $ CSS.vh 100.0
        CSS.zIndex 99998
        CSS.backgroundColor CSS.black
        case state.isActive of
          true  -> do
            CSS.display CSS.flex
            CSS.justifyContent $ CSS.JustifyContentValue 
                               $ CSS.Value 
                               $ CSS.Plain "center"
            CSS.alignItems $ CSS.AlignItemsValue 
                           $ CSS.Value 
                           $ CSS.Plain "center"
          false -> CSS.display CSS.displayNone
      , css "modal-layer" 
      ]
      [ HH.div 
        [ HCSS.style do
          CSS.position CSS.relative
          CSS.zIndex 99999
          CSS.backgroundColor CSS.white
          padding 25.0
        , css "modal-container"
        ] 
        [ HH.div
          [ HCSS.style do
            CSS.position CSS.absolute
            CSS.top $ CSS.px 0.0
            CSS.right $ CSS.px 0.0
            CSS.width $ CSS.px 25.0
            padding 8.0
          , css "modal-header" 
          , HE.onClick \_ -> Just CloseModal
          ]
          [ HH.text "X" ]
        , HH.div
          [ css "modal-body" ]
          [ state.html ]
        ]
      ]

    where
      padding :: Number -> CSS.CSS
      padding size = CSS.padding 
                     (CSS.px size)
                     (CSS.px size)
                     (CSS.px size)
                     (CSS.px size)

