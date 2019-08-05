-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production 
-- | monad, `App.AppM`.
module App.Api.Utils where

import Prelude

import Affjax (request)
import App.Api.Request (RequestOptions, defaultRequest)
import App.Capability.LogMessages (class LogMessages, logError)
import App.Capability.Now (class Now)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
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
