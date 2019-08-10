-- | The registration form allows new users to sign up to the Conduit service and authenticate
-- | their session.
module App.Page.Register where

import Preamble hiding (div)

import App.Api.Request (RegisterFields)
import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.User (class ManageUser, findUserByEmail, registerUser)
import App.Component.HTML.Layout as Layout
import App.Component.Header as Header
import App.Config (UserEnv)
import App.Data.Route (Route(..))
import App.Form.Field as Field
import App.Form.Validation as V
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Lens (set)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import FusionAuth (RegisterResponse(..))
import FusionAuth as Auth
import Halogen as H
import Halogen.Bulma as B
import Halogen.HTML.Extended as HH
import Halogen.HTML.Properties (class_, classes)
import Halogen.HTML.Properties as HP
import Record (merge)


newtype RegisterForm r f = RegisterForm (r
  ( firstName :: f V.FormError String Auth.FirstName
  , lastName :: f V.FormError String Auth.LastName
  , email :: f V.FormError String Auth.Email
  , password :: f V.FormError String Auth.Password
  ))

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

type State = Maybe RegisterResponse

data Action
  = HandleRegisterForm RegisterFields

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageUser m
  => Now m
  => LogMessages m
  => Navigate m
  => H.Component HH.HTML (Const Void) Unit Void m
component = H.mkComponent
  { initialState: const Nothing
  , render: Layout.main header <<< content
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where

  handleAction = case _ of
    HandleRegisterForm fields ->
      registerUser fields >>= Just >>> H.put

  header =
    HH.slot Header.slot unit (Header.component Register) unit absurd

  content state =
    [ HH.h1 [class_ B.title] [HH.text "Sign Up"]
    , case state of
        Nothing -> 
          signUpForm
        Just (NonUniqueUser _) ->
          HH.div_ 
          [ signUpError "User already exists!"
          , signUpForm
          ]
        Just (UserRegistered res) ->
          HH.div 
          [ classes [B.notification, B.isSuccess]]
          [ HH.text "You have been successfully signed up! \
            \Please check your email for further instructions." 
          ]
    ]

  signUpError message = HH.div 
    [ classes [B.notification, B.isDanger]]
    [ HH.text message ]

  signUpForm = HH.div 
    [ class_ B.container ]
    [ HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm) 
    ]

  formComponent :: F.Component RegisterForm (Const Void) () Unit RegisterFields m
  formComponent = F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleEvent = handleFormEvent
    }
    where

    handleFormEvent = case _ of
        evt@(F.Submitted out) -> do
          let fs = F.unwrapOutputFields out
          findUserByEmail fs.email >>= case _ of
            Just _ -> H.modify_ \st -> st { form = setEmailError st.form }
            Nothing -> F.raiseResult evt
        _ -> pure unit

    setEmailError = set (F._FieldResult proxies.email) (F.Error V.NonUniqueEmail)

    formInput :: Unit -> F.Input' RegisterForm m
    formInput _ =
      { validators: RegisterForm
        { firstName: V.required >>> V.firstNameFormat
        , lastName: V.required >>> V.lastNameFormat
        , email: V.required 
          >>> V.minLength 3 
          >>> V.emailFormat
        , password: V.required 
          >>> V.minLength 8 
          >>> V.maxLength 20 
          >>> V.passwordFormat
        }
      , initialInputs: Nothing
      }

    renderForm { validity, submitting, form } =
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
              , sym: proxies.lastName
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
          , HH.div [class_ B.level]
            [ HH.div [class_ B.levelLeft]
              [ HH.div [class_ B.levelItem] [Field.submit validity submitting "Sign up"]
              ]
            , HH.div [class_ B.levelRight]
              [ HH.div [class_ B.levelItem] 
                [ HH.p_ [HH.a [HH.safeHref Login] [HH.text "Already have an account?"]]
                ]
              ]
            ] 
          ]
        ]
    proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)
