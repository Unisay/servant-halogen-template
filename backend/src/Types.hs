module Types
  ( module Data.BaseUrl
  , TcpPort
  , mkTcpPort
  , unTcpPort
  , Config (..)
  , Environment (..)
  ) where

import Data.BaseUrl
import Preamble

import Control.Concurrent   (ThreadId)
import System.Envy.Extended (ShowableVar (..), Var (..))


newtype TcpPort
  = TcpPort Int
  deriving newtype (Eq, Show, Var)

mkTcpPort :: Int -> Maybe TcpPort
mkTcpPort port | port >= 0 && port < 65536 = Just $ TcpPort port
mkTcpPort _ = Nothing

unTcpPort :: TcpPort -> Int
unTcpPort (TcpPort port) = port


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
