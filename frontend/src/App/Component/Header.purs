module App.Component.Header 
  ( component
  , Query (..)
  , Input
  , Output
  , Slot
  , slot
  ) where

import Preamble hiding (div)

import App.Capability.Navigate (class Navigate, logout)
import App.Component.Utils (busEventSource)
import App.Config (UserEnv)
import App.Data.Route (Route(..))
import CSS (bold, fontFamily, fontSize, fontWeight, pt, sansSerif)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Const (Const)
import Data.Foldable (intercalate)
import Data.Monoid (guard)
import Data.NonEmpty (singleton)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FusionAuth (User, printFirstName, printLastName)
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Extended (HTML, dataAttr, span) as HH
import Halogen.HTML.Extended (a, className, css, div, maybeElem, nav, safeHref, strong_, text, whenElem)
import Halogen.HTML.Properties (class_, classes, id_)
import Halogen.HTML.Properties.ARIA as ARIA


type Query = Const Void
type Input = Unit
type Output = Void
type Slot = H.Slot Query Output
type State = { isOpen :: Boolean, currentUser :: Maybe User }
data Action 
  = Initialize
  | ToggleMenu
  | HandleUserBus (Maybe User)
  | Logout

slot :: SProxy "header"
slot = SProxy

component :: forall m r
   . MonadEffect m 
  => MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => Route
  -> H.Component HH.HTML Query Input Output m
component route' = H.mkComponent
  { initialState: \_ -> { isOpen: false, currentUser: Nothing }
  , render: \state -> render route' state
  , eval: H.mkEval $ H.defaultEval 
    { handleAction = handleAction 
    , initialize = pure Initialize
    }
  }

  where

  handleAction :: Action -> H.HalogenM State Action _ Void m Unit
  handleAction = case _ of 
    Initialize ->  do
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbUser <- liftEffect $ Ref.read currentUser
      H.modify_ _ { currentUser = mbUser }
    ToggleMenu -> do
      liftEffect $ log "Toggling menu"
      H.modify_ \s -> s { isOpen = not s.isOpen }
    Logout -> 
      logout
    HandleUserBus user ->
      H.modify_ _ { currentUser = user }

  render :: Route -> State -> H.ComponentHTML _ _ m
  render route { isOpen, currentUser } = nav
    [ class_ B.navbar, ARIA.role "navigation", ARIA.label "main navigation" ] 
    [ div [class_ B.navbarBrand] [logo, burger]
    , div 
      [ id_ "navbar-menu"
      , classes $ guard isOpen [B.isActive] <> [B.navbarMenu]
      ]
      [ div [class_ B.navbarStart]
        [ navItem Home [text "Home"]
        ]
      , div [class_ B.navbarEnd]
        [ maybeElem currentUser \user ->
            div [class_ B.navbarItem]
              [ text $ intercalate " " 
                [ "Signed in as "
                , maybe "" printFirstName user.firstName 
                , maybe "" printLastName user.lastName
                ]
              ]
        , maybeElem currentUser \user ->
            div [class_ B.navbarItem]
              [ a [classes [B.button, B.isPrimary], HE.onClick $ const $ Just Logout]
                    [ strong_ [text "Sign out"] ]
              ]
        , whenElem (isNothing currentUser) \_ -> 
            div [class_ B.navbarItem]
              [ div [className "buttons"]
                [ a [classes [B.button, B.isPrimary], safeHref Register]
                  [ strong_ [text "Sign up"] ]
                , a [classes [B.button, B.isLight], safeHref Login][text "Log in"]
                ]
              ]
        ]
      ]
    ]

    where

    navItem :: forall i p. Route -> Array (HH.HTML i p) -> HH.HTML i p
    navItem r html = 
      div 
      [class_ B.navbarItem]
      [ a [ css $ guard (route == r) " active", safeHref r ] html ]

    logo :: forall i p. HH.HTML i p
    logo = a 
      [class_ B.navbarItem, safeHref Home, logoStyle] 
      [text "Halogen 5 Template"]
      where
      logoStyle = style do 
        fontFamily ["Galada"] (singleton sansSerif)
        fontWeight bold
        fontSize (pt 20.0)

    burger :: forall i. HH.HTML i Action
    burger = a 
      [ classes [B.navbarBurger, B.burger]
      , HH.dataAttr "target" "navbar-menu"
      , HE.onClick \_ -> pure ToggleMenu
      , ARIA.role "button"
      , ARIA.label "menu"
      , ARIA.expanded "false"
      ]
      [ HH.span [ARIA.hidden "true"] []
      , HH.span [ARIA.hidden "true"] []
      , HH.span [ARIA.hidden "true"] []
      ]