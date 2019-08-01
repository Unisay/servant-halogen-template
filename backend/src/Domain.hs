module Domain
  ( User
  , UserId
  , UserData
  , FirstName
  , SecondName
  , Email
  , mkEmail
  , unEmail
  ) where

import           Preamble

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time  (UTCTime)

import qualified Data.Text  as T


newtype UserId
  = UserId Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, ToJSON, FromJSON)

newtype FirstName
  = FirstName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

newtype SecondName
  = SecondName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

newtype Email
  = Email Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

mkEmail :: Text -> Maybe Email
mkEmail email | not (T.null email) = Just $ Email email
mkEmail _     = Nothing

unEmail :: Email -> Text
unEmail (Email email) = email

data UserData
  = UserData
  { _userDataFirstName  :: FirstName
  , _userDataSecondName :: SecondName
  , _userDataEmail      :: Email
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON)

data User
  = User
  { _userId         :: UserId
  , _userFirstName  :: FirstName
  , _userSecondName :: SecondName
  , _userEmail      :: Email
  , _registeredAt   :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON)
