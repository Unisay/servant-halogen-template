-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module App.Page.Register where

import Preamble hiding (div)

import App.Api.Request (RegisterFields)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.User (class ManageUser, registerUser)
import App.Component.HTML.Layout as Layout
import App.Component.Header as Header
import App.Data.Email (Email)
import App.Data.Route (Route(..))
import App.Data.User (FirstName, SecondName)
import App.Form.Field as Field
import App.Form.Validation as V
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.Extended (a, div, h1, p_, safeHref, text)
import Halogen.HTML.Extended as HH
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Record (merge)

newtype RegisterForm r f = RegisterForm (r
  ( firstName :: f V.FormError String FirstName
  , secondName :: f V.FormError String SecondName
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = HandleRegisterForm RegisterFields

component
  :: forall m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState: const unit
  , render: \_ -> Layout.main header content
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where

  handleAction = case _ of
    HandleRegisterForm fields ->
      registerUser fields >>= traverse_ (\_ -> navigate Home)

  header =
    HH.slot Header.slot unit (Header.component Register Nothing) unit absurd

  content =
    [ h1 [class_ B.title] [text "Sign Up"]
    , signUpForm
    ]

  signUpForm = HH.div 
    [ class_ B.container ]
    [ HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm) 
    ]

  formComponent :: F.Component RegisterForm (Const Void) () Unit RegisterFields m
  formComponent = F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleEvent = F.raiseResult
    }
    where
    formInput :: Unit -> F.Input' RegisterForm m
    formInput _ =
      { validators: RegisterForm
          { firstName: V.required >>> V.firstNameFormat
          , secondName: V.required >>> V.secondNameFormat
          , email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 8 >>> V.maxLength 20
          }
      , initialInputs: Nothing
      }

    renderForm { form } =
      HH.form_
        [ HH.fieldset_
          [ Field.input $
              { label: "First Name"
              , sym: proxies.firstName
              , props: [HP.type_ HP.InputText]
              , form
              } `merge` Field.inputDefaults
          , Field.input $
              { label: "Second Name"
              , sym: proxies.secondName
              , props: [HP.type_ HP.InputText]
              , form
              } `merge` Field.inputDefaults
          , Field.input $ 
              { label: "E-mail"
              , sym: proxies.email
              , props: [HP.type_ HP.InputEmail]
              , form
              } `merge` Field.inputDefaults
          , Field.input $
              { label: "Password"
              , hint: Just "At least 8 characters"
              , sym: proxies.password
              , props: [HP.type_ HP.InputPassword]
              , form
              } `merge` Field.inputDefaults
          , div [class_ B.level]
            [ div [class_ B.levelLeft]
              [ div [class_ B.levelItem] [Field.submit "Sign up"]
              ]
            , div [class_ B.levelRight]
              [ div [class_ B.levelItem] 
                [ p_ [a [safeHref Login] [text "Already have an account?"]]
                ]
              ]
            ] 
          ]
        ]
      where
      proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)
