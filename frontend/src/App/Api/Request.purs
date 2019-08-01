-- | For now, our production app manages resources and fetches data using a REST API. This module
-- | defines several data types and helper functions to create and manage these requests. That
-- | includes managing auth tokens, designing types to represent possible requests, and more.
-- | 
-- | While interesting, this module is largely mechanical. It helps provide most of the low-level
-- | functions that our production monad will leverage in `Conduit.AppM` to implement our various
-- | app capabilities.
module App.Api.Request 
  ( RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , Unlifted(..)
  , RegisterFields(..)
  , LoginFields(..)
  , AuthFieldsRep(..)
  , login
  , register
  ) where

import Prelude

import Affjax (Request, printResponseFormatError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import App.Api.Endpoint (Endpoint(..), endpointCodec)
import App.Data.Email (Email)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import FusionAuth as Auth
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | A type to represent the different methods our API supports. An API request
-- | can be GET, POST, PUT, or DELETE. These methods are already captured by the `Data.HTTP.Method`
-- | module, but I've opted for a custom type because, in our API, only `Post` and `Put` can have
-- | optional JSON bodies. There should never be a `Get` request with a JSON body. This type 
-- | prevents that from occurring.
data RequestMethod 
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- | The base url and token are necessary to make a request, but are low-level implementation
-- | details. When actually using the API, we generally worry about which endpoint we'd like to 
-- | access and what request method we'd like to use.
type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: Auth.ApiUrl -> Maybe Auth.ApiKey -> RequestOptions -> Request Json
defaultRequest (Auth.ApiUrl apiUrl) auth { endpoint, method } =
  { method: Left method 
  , url: Auth.unApiUrl apiUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Auth.Token t) -> [ RequestHeader "Authorization" $ "Auth.Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- | The following data types and functions aren't a natural fit for this module, but they've 
-- | been included here because they operate on tokens. Remember: we can't create or manipulate 
-- | the `Auth.Token` type outside this module, so we'll somewhat awkwardly define the registration
-- | and login requests in this module. These requests will be the only way to create an auth
-- | token in the system.

-- | First, we'll define a few types as the inputs for our login and authentication requests.
-- |
-- | Both functions -- as well as functions from the `Conduit.Capability.Resource.User` module -- 
-- | share most of their required input fields. Rather than define three separate record types
-- | (which could fall out of sync with one another and introduce unnecessary boilerplate), we'll
-- | use PureScript's lovely extensible row types to share as much as possible among the inputs.
-- |
-- | This is possible because rows are a type-level list of keys and values, and these lists
-- | can be merged. Here's an example:
-- |
-- | ```purescript
-- | -- A row containing a label, "a" with a value of type `Int`, which can be extended with 
-- | -- another row
-- | type RowA r = (a :: Int | r) 
-- | 
-- | -- A row containing all labels from `RowA`, plus an additional label "b" with a value of 
-- | type `Int`, which can be extended by yet another row. The two versions below are equivalent.
-- | type RowAB r = (b :: Int | RowA r)
-- | type RowAB r = (a :: Int, b :: Int | r)
-- |
-- | -- A "closed" row, which simply means it cannot be extended with further rows.
-- | type RowABC = (c :: Int | RowA RowB)
-- |
-- | -- Records are really just rows under the hood, with nicer syntax
-- | type RecordABC = Record RowABC
-- | type RecordABC = { | RowABC }
-- | type RecordABC = { a :: Int, b :: Int, c :: Int }
-- | ```

-- | We're almost ready to get started, but we have a snag! We're going to define two record types 
-- | using most of the same underlying row. But in some records, we'll have a password field with 
-- | the type `Maybe String`, but in others, we'll have a password field with the type `String`. 
-- | How can we still share these rows?
-- | 
-- | We can reconcile this by defining our row so that the password field is in a "box" represented 
-- | by a type variable, `box`. This box can be filled in with various types of the kind 
-- | `Type -> Type`, like `Maybe a`, `Array a`, `List a`, and so on. For example:
-- |
-- | ```purescript
-- | type MyRecord box = { myValue :: box String }
-- | 
-- | myMaybeRecord :: MyRecord Maybe
-- | myMaybeRecord = { myValue: Just "" }
-- |
-- | myArrayRecord :: MyRecord Array
-- | myArrayRecord = { myValue: [ "", "", "" ] }
-- | ```
-- |
-- | We still need to be able to produce this record, however:
-- | 
-- | ```purescript
-- | myRecord :: MyRecord ???
-- | myRecord = { myValue: "" }
-- | ```
-- |
-- | How can we accomplish this? We've asserted that the `String` exists within some container,
-- | `box`, but it looks like we now need the `String` type NOT in a containing type.
-- |
-- | We can reconcile this with the `Unlifted` type synonym defined below. It's similar to the
-- | `identity` function in that it just hands back the type you provide. It's like a box that
-- | erases itself at compile-time. The correct version of `myRecord` is this:
-- |
-- | ```purescript
-- | myRecord :: MyRecord Unlifted
-- | myRecord = { myValue: "" }
-- | ```
type Unlifted a = a 

-- | Now we can define a shared row for various requests which manage user credentials. And the 
-- | password field can be a `Maybe String` or a `String`, depending on what we need!
-- |
-- | By convention, I give row types that will later be used as records the `-Rep` suffix.
type AuthFieldsRep box r = ( email :: Email, password :: box String | r )

-- | Our login and registration records both require a password field with a `String` value. 
-- | However, in the `Conduit.Capability.Resource.User` module, we'll see another record using  
-- | the same fields in which the password is a `Maybe String`. I encourage you to check it out
-- | to see this pattern in practice!
-- |
-- | The types below are equivalent to:
-- |
-- | ```purescript
-- | type RegisterFields = { email :: Email, password :: String, username :: Username }
-- | type LoginFields = { email :: Email, password :: String }
-- | ```
-- | 
-- | Admittedly, it's not strictly necessary to share fields among the three auth types because
-- | they're so small. But this pattern is common in production applications and in libraries like
-- | `purescript-record`, `purescript-variant`, and `purescript-halogen-formless`, so it's
-- | important to be aware of it.
type RegisterFields = 
  {| AuthFieldsRep Unlifted (firstName :: Auth.FirstName, lastName :: Auth.LastName) }

type LoginFields = 
  {| AuthFieldsRep Unlifted () }

-- | This function logs a user in (if they exist), 
-- | returning an auth token and the user's minimal profile.
login :: forall m. MonadAff m 
  => Auth.ApiUrl -> LoginFields -> m (Either String (Tuple Auth.Token Auth.User))
login apiUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields } 
   in requestUser apiUrl { endpoint: Login, method }

-- | This function registers a user (if they don't already exist), 
-- | returning an auth token and the user's minimal profile.
register :: forall m. MonadAff m 
  => Auth.ApiUrl -> RegisterFields -> m (Either String (Tuple Auth.Token Auth.User))
register apiUrl fields = 
  let method = Post $ Just $ encodeJson { user: fields }
   in requestUser apiUrl { endpoint: Users, method } 
