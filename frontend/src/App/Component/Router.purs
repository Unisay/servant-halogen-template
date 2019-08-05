-- | The `Router` component is the root of our Halogen application. 
-- | Every other component is a direct descendent of this component. 
-- | We'll use the router to choose which component to render given a particular
-- | `Route` and to manage the user's location in the application.
-- |
module App.Component.Router where

import Preamble

import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.User (class ManageUser, User)
import App.Component.Utils (OpaqueSlot, busEventSource)
import App.Data.Route (Route(..), routeCodec)
import App.Config (UserEnv)
import App.Page.Home as Home
import App.Page.Login as Login
import App.Page.Register as Register
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (hush)
import Data.Foldable (elem)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Routing.Hash (getHash)

type State =
  { route :: Maybe Route 
  , currentUser :: Maybe User
  }

data Query a
  = Navigate Route a

data Action 
  = Initialize 
  | HandleUserBus (Maybe User)

type ChildSlots = 
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , user :: OpaqueSlot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML Query Unit Void m
component = H.mkComponent
  { initialState: \_ -> { route: Nothing, currentUser: Nothing } 
  , render
  , eval: H.mkEval $ H.defaultEval 
    { handleQuery = handleQuery 
    , handleAction = handleAction
    , initialize = Just Initialize
    }
  }
  where 
  
  homeSlot = SProxy :: _ "home"
  loginSlot = SProxy :: _ "login"
  registerSlot = SProxy :: _ "register"
  userSlot = SProxy :: _ "User"

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first, we'll get the value of the current user and 
      -- subscribe to updates any time the value changes
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbUser <- liftEffect (Ref.read currentUser) 
      H.modify_ _ { currentUser = mbUser }
      -- then, we'll get the route the user landed on
      initialRoute <- hush <<< RD.parse routeCodec <$> liftEffect getHash
      -- and, finally, we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute
    
    HandleUserBus mbUser -> do
      H.modify_ _ { currentUser = mbUser }

  handleQuery :: forall a. Query a 
    -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get 
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ Login, Register ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  -- Display the login page instead of the expected page 
  -- (if there is no current user) a simple way to restrict access.
  authorize 
    :: Maybe User 
    -> H.ComponentHTML Action ChildSlots m 
    -> H.ComponentHTML Action ChildSlots m
  authorize mbUser html = case mbUser of
    Nothing ->
      HH.slot loginSlot unit Login.component { redirect: false } absurd
    Just _ ->
      html
   
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home -> 
        HH.slot homeSlot unit Home.component unit absurd
      Login -> 
        HH.slot loginSlot unit Login.component { redirect: true } absurd
      Register -> 
        HH.slot registerSlot unit Register.component unit absurd
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
      