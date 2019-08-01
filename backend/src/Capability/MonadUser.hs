module Capability.MonadUser
  ( MonadUser (..)
  ) where

import Preamble

import Domain         (User, UserData)
import Servant.Server (Handler)


class MonadUser m where
  register :: UserData -> m User

instance MonadUser Handler where
  register :: UserData -> Handler User
  register = notImplemented
