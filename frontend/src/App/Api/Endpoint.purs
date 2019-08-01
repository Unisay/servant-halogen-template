-- | Application uses a REST API for resource management. This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile. Since the library we use to perform requests
-- | uses string URLs, we need to be able to write our endpoints to string values. We'll use the
-- | `routing-duplex` library to get this string conversion for free.
-- |
-- | In a larger application we might code-generate this module from an Open API or Swagger 
-- | spec, or split it into several separate modules.
module App.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Login
  | User
  | Users

derive instance genericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where show = genericShow

-- | For a full treatment of how this function produces both a parser and printer guaranteed to
-- | produce valid paths, see the `routing-duplex` tutorial:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "users" / "login" / noArgs 
  , "User": "user" / noArgs
  , "Users": "users" / noArgs
  }
