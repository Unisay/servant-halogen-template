-- | This module provides small utilities for building form fields with Formless. They're not
-- | necessary to use the library, but help alleviate a little boilerplate around handling 
-- | inputs. To read more about how to use the Formless library, see:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
-- |
-- | In a framework like React, little bundles of functionality like this might be individual 
-- | components. In Halogen, they're simple pure functions which produce HTML. 
module App.Form.Field where

import Preamble hiding (div)

import App.Form.Validation (errorToString)
import App.Form.Validation as V
import DOM.HTML.Indexed (HTMLinput)
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Formless as F
import Halogen.Bulma as B
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Extended (ClassName(..), HTML, div, i, label, maybeElem, p, span, text)
import Halogen.HTML.Properties (class_, classes)
import Halogen.HTML.Properties as HP
import Type.Row as Row

-- | Formless (the form library for Halogen) supports a submit event which will attempt to validate
-- | and return the successfully-parsed fields. We can create a small helper function that creates
-- | a submit button with customizable text and the submit event triggered by a click. Since all 
-- | submit buttons in the application look the same, we can jus use this throughout the app.
submit :: forall form act slots m. String -> F.ComponentHTML form act slots m
submit buttonText = 
  div 
    [ class_ B.control ]
    [ HH.button
      [ classes [B.button, B.isLink], HE.onClick \_ -> Just F.submit ]
      [ text buttonText ]
    ]

inputDefaults :: 
  { hint      :: Maybe String
  , leftIcon  :: Maybe InputIcon
  , rightIcon :: Maybe InputIcon
  } 
inputDefaults = 
  { hint: Nothing
  , leftIcon: Nothing
  , rightIcon: Nothing
  }

-- | This helper function creates an input field hooked up with Formless, including styles,
-- | events, error handling, and more. The unction ensures at compile-time that the field we 
-- | want actually exists in the form, that the input, error, and output types of the field are 
-- | compatible, that the only properties you attempt to set on the HTML are actual valid <input> 
-- | properties, and more.
-- |
-- | Let's deconstruct the type.
-- |
-- | First, the `IsSymbol` constraint requires that our first argument, `sym`, is a type-level
-- | string. You've seen these all over the place -- record labels are one example. We'll use 
-- | this any time we need to talk about a value existing at a particular key in a record or
-- | a variant.
-- |
-- | Next, the two `Newtype` constraints require that you can use the `unwrap` function to 
-- | transform the first type into the second type. In other words, the first type has to have
-- | a `Newtype` instance. This is how we'll unpack our self-defined Formless form type into
-- | either a raw record or variant we can work with.
-- |
-- | Next, the two `Cons` constraints require that there exists a value of the type given in
-- | the second argument at the label `sym` in the record or variant given in the last argument.
-- | For instance, we require that there's a field with an error type `FormError` and an input
-- | type `String` at the label `sym` in the row `fields`. In short, we require at compile-time
-- | that an input field of the correct type exists in our form state at the key we provided as  
-- | the function's first argument.
input 
  :: forall form act slots m sym fields inputs out t0 t1
   . IsSymbol sym
  => Newtype (form Record F.FormField) { | fields }
  => Newtype (form Variant F.InputFunction) (Variant inputs)
  => Row.Cons sym (F.FormField V.FormError String out) t0 fields
  => Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs
  => { label     :: String 
     , hint      :: Maybe String
     , leftIcon  :: Maybe InputIcon
     , rightIcon :: Maybe InputIcon
     , sym       :: SProxy sym
     , form      :: form Record F.FormField 
     , props     :: Array (HH.IProp HTMLinput (F.Action form act))
     }   
  -> F.ComponentHTML form act slots m
input arg =
  div 
    [ class_ B.field ]
    [ label [class_ B.label][text arg.label]
    , div 
        [ classes $ [B.control] 
          <> guard (isJust arg.leftIcon)  [B.hasIconsLeft]
          <> guard (isJust arg.rightIcon) [B.hasIconsRight]
        ]
        [ HH.input $
          [ class_ B.input
          , HP.value $ F.getInput arg.sym arg.form
          , HE.onValueInput $ Just <<< F.setValidate arg.sym
          ] <> arg.props
        , maybeElem arg.leftIcon \ico ->
            span [classes [B.icon, B.isSmall, B.isLeft]] [icon ico]
        , maybeElem arg.rightIcon \ico ->
            span [classes [B.icon, B.isSmall, B.isRight]] [icon ico]
        ]
    , case F.getError arg.sym arg.form of
        Just e ->
          p [classes [B.help, B.isDanger]] [text $ errorToString e]
        Nothing -> case arg.hint of
          Just h ->
            p [class_ B.help] [text h]
          Nothing ->
            p [class_ B.help] []
    ]

data InputIcon = Envelope | Lock | User

icon :: forall i p. InputIcon -> HTML i p
icon = case _ of
  Envelope -> cs "envelope" 
  Lock -> cs "lock" 
  User -> cs "user"
  where cs name = i [classes [ClassName "fas", ClassName $ "fa-" <> name]][]
     