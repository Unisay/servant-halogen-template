-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production 
-- | monad, `App.AppM`.
module App.Api.Utils where

import Prelude

import Affjax (request)
import App.Api.Request (RequestOptions, defaultRequest)
import App.Capability.LogMessages (class LogMessages, logError)
import App.Capability.Now (class Now)
import App.Data.Profile (Profile)
import App.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, ask, asks)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import FusionAuth as Auth


-- | This function performs a request that does not require authentication by pulling the base URL 
-- | out of the app environment and running an asynchronous request. This function only requires the 
-- | `apiUrl` field from the app environment. See `Conduit.AppM` for examples of this in action.
mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { apiUrl :: Auth.ApiUrl | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { apiUrl } <- ask
  response <- liftAff $ request $ defaultRequest apiUrl Nothing opts
  pure $ hush response.body

-- | Logging in and registering share a lot of behavior, namely updating the application environment
-- | and writing the auth token to local storage. This helper function makes it easy to layer those
-- | behaviors on top of the request. This also performs the work of broadcasting changes in the
-- | current user to all subscribed components.
authenticate 
  :: forall m a r
   . MonadAff m
  => MonadAsk { apiUrl :: Auth.ApiUrl, userEnv :: UserEnv | r } m
  => LogMessages m
  => Now m
  => (Auth.ApiUrl -> a -> m (Either String (Tuple Auth.Token Profile))) 
  -> a 
  -> m (Maybe Profile)
authenticate req fields = do 
  { apiUrl, userEnv } <- ask
  req apiUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do 
      liftEffect do 
        writeToken token 
        Ref.write (Just profile) userEnv.currentUser
      -- any time we write to the current user ref, we should also broadcast the change 
      liftAff $ Bus.write (Just profile) userEnv.userBus
      pure (Just profile)

-- | We can decode records and primitive types automatically, and we've defined custom decoders for
-- | our custom data types. However, our API frequently returns those data structures wrapped in 
-- | a larger object with a single field like "user", "profile", or "article". This utility allows
-- | us to decode a JSON object with a particular key, and then decode the contents. 
-- |
-- | For example, consider this JSON object containing a single field, "user", which itself contains 
-- | a JSON object representing a user profile:
-- |
-- | ```json
-- | { "user": { "username": ... } }
-- | ```
-- | 
-- | We can make our `Profile` decoder compatible with this new JSON using our `decodeAt` helper:
-- |
-- | ```purescript
-- | decodeProfile :: Json -> Either String Profile
-- | decodeProfile = decodeAt "user"
-- | ```
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt = decodeWithAt decodeJson

decodeWithAt :: forall a. DecodeJson a => (Json -> Either String a) -> String -> Json -> Either String a
decodeWithAt decoder key = decoder <=< (_ .: key) <=< decodeJson

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed 
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of 
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing 
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)
