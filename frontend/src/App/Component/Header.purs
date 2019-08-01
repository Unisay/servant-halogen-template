module App.Component.Header 
  ( component
  , Query (..)
  , Input
  , Output
  , Slot
  , slot
  ) where

import Preamble hiding (div)

import App.Data.Profile (Profile)
import App.Data.Route (Route(..))
import CSS (bold, fontFamily, fontSize, fontWeight, pt, sansSerif)
import Data.Const (Const)
import Data.Monoid (guard)
import Data.NonEmpty (singleton)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Extended (HTML, dataAttr, span) as HH
import Halogen.HTML.Extended (a, className, css, div, nav, safeHref, strong_, text)
import Halogen.HTML.Properties (class_, classes, id_)
import Halogen.HTML.Properties.ARIA as ARIA


type Query = Const Void
type Input = Unit
type Output = Void
type Slot = H.Slot Query Output
data Action = ToggleMenu
type State = { isOpen :: Boolean }

slot :: SProxy "header"
slot = SProxy

component :: forall m
   . MonadEffect m 
  => Route
  -> Maybe Profile
  -> H.Component HH.HTML Query Input Output m
component route' currentUser' = H.mkComponent
  { initialState: \_ -> { isOpen: false }
  , render: render route' currentUser'
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

  where

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction ToggleMenu = do
    liftEffect $ log "Toggling menu"
    H.modify_ \s -> s { isOpen = not s.isOpen }

  render :: Route -> Maybe Profile -> State -> H.ComponentHTML _ _ m
  render route currentUser { isOpen } = nav
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
        [ div [class_ B.navbarItem]
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