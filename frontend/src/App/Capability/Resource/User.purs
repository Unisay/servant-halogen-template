-- | A capability representing the ability to manage users in our system. 
-- | That includes logging in, registering and more.
module App.Capability.Resource.User where

import Prelude

import App.Api.Request (AuthFieldsRep, LoginFields, RegisterFields)
import App.Data.Profile (Profile, ProfileRep, ProfileWithEmail)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Type.Row (type (+))

-- | This type is a record made up of two row types: 
-- | the fields that make up a profile, plus the fields used for authentication, 
-- | like their email address and password. 
-- | See the `App.Data.Profile` module for more details.
type UpdateProfileFields = { | ProfileRep + AuthFieldsRep Maybe () }

-- | This capability represents the ability to manage users in our system. 
-- | We support logging users in, and registering them, 
-- | as well as reading information about various users.
-- | 
-- | We'll handle all the mechanics of making the request, decoding responses, 
-- | handling errors, and so on in the implementation.
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe ProfileWithEmail)
  updateUser :: UpdateProfileFields -> m Unit

-- | This instance lets us avoid having to use `lift` 
-- | when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m 
  => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  updateUser = lift <<< updateUser
