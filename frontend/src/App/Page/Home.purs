-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module App.Page.Home where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Component.HTML.Layout as Layout
import App.Component.Header as Header
import App.Component.Utils (busEventSource)
import App.Config (UserEnv)
import App.Data.Route (Route(..))
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import FusionAuth (User)
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.Extended (h1, h2, text)
import Halogen.HTML.Extended as HH
import Halogen.HTML.Properties (class_)

data Action = Initialize | HandleUserBus (Maybe User)

type ChildSlots = (header :: Header.Slot Unit)

type State = { currentUser :: Maybe User, page :: Int }

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  
  initialState :: Unit -> State
  initialState _ = { currentUser: Nothing, page: 1 }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ _
  handleAction = case _ of
    Initialize -> do
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      pure unit

    HandleUserBus user ->
      H.modify_ _ { currentUser = user }

  render :: State -> H.ComponentHTML _ _ m
  render state@{ currentUser } = Layout.main header content
    where

    header = 
      HH.slot Header.slot unit (Header.component Home) unit absurd

    content =
      [ HH.div [class_ B.heroBody]
        [ HH.div [class_ B.container] 
          [ h1 [class_ B.title] 
            [text "Full-stack project template"]
          , h2 [class_ B.subtitle] 
            [text "With Haskell on the backend and PureScript on the frontend"] 
          ]
        ]
      ]

