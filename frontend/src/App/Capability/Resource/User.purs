-- | A capability representing the ability to manage users in our system. 
-- | That includes logging in, registering and more.
module App.Capability.Resource.User where

import Prelude

import App.Api.Request (LoginFields, RegisterFields)
import Data.Maybe (Maybe)
import FusionAuth as Auth
import Halogen (HalogenM, lift)

type User = Auth.UserIn

-- | This capability represents the ability to manage users in our system. 
-- | We support logging users in, and registering them.
-- | 
-- | We'll handle all the mechanics of making the request, decoding responses, 
-- | handling errors, and so on in the implementation.
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe User)
  registerUser :: RegisterFields -> m (Maybe User)

-- | This instance lets us avoid having to use `lift` 
-- | when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m 
  => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
