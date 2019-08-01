-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module App.Page.Login where

import Preamble hiding (div)

import App.Api.Request (LoginFields)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.User (class ManageUser, loginUser)
import App.Component.HTML.Layout as Layout
import App.Component.Header as Header
import App.Data.Email (Email)
import App.Data.Route (Route(..))
import App.Form.Field as Field
import App.Form.Validation as V
import Data.Const (Const)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.Extended (a, div, fieldset_, h1, p_, safeHref, text, whenElem)
import Halogen.HTML.Extended as HH
import Halogen.HTML.Properties (class_, classes, type_)
import Halogen.HTML.Properties as HP
import Record (merge)


data Action = HandleLoginForm LoginFields

-- Should this component redirect to home after login or not? If the login page is loaded
-- at the login route, then yes; if not, then it is guarding another route and should not.
type State = { redirect :: Boolean }

type Input = { redirect :: Boolean }

type ChildSlots =
  ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit 
  , header :: Header.Slot Unit
  )

component
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
  { initialState: identity
  , render: \_ -> Layout.main header content 
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where

  handleAction = case _ of
    HandleLoginForm fields -> do
      -- loginUser also handles broadcasting the user changes to subscribed 
      -- components so they receive the up-to-date value 
      -- (see AppM and the `authenticate` function.)
      loginUser fields >>= case _ of
        Nothing ->
          void $ H.query F._formless unit 
               $ F.injQuery 
               $ SetLoginError true unit
        Just profile -> do
          void $ H.query F._formless unit 
               $ F.injQuery 
               $ SetLoginError false unit
          st <- H.get
          when st.redirect (navigate Home)

  header =
    HH.slot Header.slot unit (Header.component Login Nothing) unit absurd 

  content = 
    [ h1 [class_ B.title] [text "Sign In"]
    , form
    ]

  form = HH.div
    [ class_ B.container ]
    [ HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
    ]


-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless

newtype LoginForm r f = LoginForm (r
  ( email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

-- We can extend our form to receive more queries than it supports by default. Here, we'll
-- set a login error from the parent.
data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

formComponent :: forall m
   . MonadAff m 
  => F.Component LoginForm FormQuery () Unit LoginFields m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderLogin
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  }
  where
  formInput :: Unit -> F.Input LoginForm (loginError :: Boolean) m
  formInput _ =
    { validators: LoginForm
        { email: V.required >>> V.minLength 3 >>> V.emailFormat
        , password: V.required >>> V.minLength 2 >>> V.maxLength 20
        }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      H.modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  renderLogin { form, loginError } =
    HH.form_
      [ whenElem loginError \_ ->
          div
            [ classes [B.notification, B.isDanger] ]
            [ text "Email or password is invalid" ]
      , fieldset_
          [ Field.input $
              { label: "E-mail"
              , sym: proxies.email
              , props: [HP.type_ HP.InputEmail]
              , form
              } `merge` Field.inputDefaults
          , Field.input $
              { label: "Password"
              , sym: proxies.password
              , props: [class_ B.input, type_ HP.InputPassword]
              , form
              } `merge` Field.inputDefaults
          , div [class_ B.level]
            [ div [class_ B.levelLeft]
              [ div [class_ B.levelItem] [Field.submit "Log in"]
              ]
            , div [class_ B.levelRight]
              [ div [class_ B.levelItem] 
                [ p_ [a [safeHref Register] [text "Need an account?"]]
                ]
              ]
            ] 
          ]
      ]
