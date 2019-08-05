-- | Halogen does not support writing an HTML string to the DOM. This component allows us to do this
-- | at a particular controlled HTML node.
module App.Component.RawHTML where

import Prelude

import App.Foreign.Marked (RawHTML, marked)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

-- | For an explanation of how to properly use the PureScript FFI with JavaScript, please see the 
-- | `src/Foreign/Marked.js` file and the `App.Foreign.Marked` module.
foreign import unsafeSetInnerHTML :: HTMLElement -> RawHTML -> Effect Unit

type State =
  { elemRef :: H.RefLabel
  , markdown :: String
  }

type Input =
  { markdown :: String }

data Action
  = SetInnerHTML
  | Receive Input

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: \{ markdown } -> { elemRef: H.RefLabel "markdown", markdown } 
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction 
      , receive = Just <<< Receive 
      , initialize = Just SetInnerHTML
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    SetInnerHTML -> do
      { elemRef } <- H.get
      mbElem <- H.getHTMLElementRef elemRef
      for_ mbElem \el -> do  
        { markdown } <- H.get
        H.liftEffect $ unsafeSetInnerHTML el $ marked markdown
    
    Receive { markdown } -> do
      H.modify_ _ { markdown = markdown }
      handleAction SetInnerHTML

  render :: State -> H.ComponentHTML Action () m
  render state = HH.div [ HP.ref state.elemRef ] []
