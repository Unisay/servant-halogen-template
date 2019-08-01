module Server.Handlers
  ( users
  ) where

import Capability.MonadUser (MonadUser (..))
import Domain               (User, UserData)


users :: MonadUser m => UserData -> m User
users = register
