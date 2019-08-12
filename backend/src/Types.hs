module Types
  ( module Data.BaseUrl
  , module Data.TcpPort
  , Config (..)
  , Environment (..)
  ) where

import Data.BaseUrl
import Data.TcpPort
import Preamble

import Control.Concurrent   (ThreadId)
import System.Envy.Extended (ShowableVar (..), Var (..))


data Config
  = Config
  { _configApiServerPort :: TcpPort
  , _configEnvironment   :: Environment
  , _configEkgServer     :: ThreadId
  , _configAuthApi       :: BaseUrl
  } deriving stock (Eq, Show)

data Environment
  = Dev
  | Prod
  deriving stock (Eq, Show, Read)
  deriving Var via ShowableVar Environment
