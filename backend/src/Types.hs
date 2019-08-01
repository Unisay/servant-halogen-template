module Types
  ( TcpPort
  , mkTcpPort
  , unTcpPort
  , Config (..)
  , Environment (..)
  ) where

import Preamble
import System.Envy.Extended (Var, ShowableVar(..))
import Control.Concurrent (ThreadId)


newtype TcpPort
  = TcpPort Int
  deriving newtype (Eq, Show, Var)

mkTcpPort :: Int -> Maybe TcpPort
mkTcpPort port | port >= 0 && port < 65536 = Just $ TcpPort port
mkTcpPort _    = Nothing

unTcpPort :: TcpPort -> Int
unTcpPort (TcpPort port) = port


data Config
  = Config
  { _configApiServerPort :: TcpPort
  , _configEnvironment :: Environment
  , _configEkgServer :: ThreadId
  } deriving stock (Eq, Show)

  
data Environment
  = Dev
  | Prod
  deriving stock (Eq, Show, Read)
  deriving Var via ShowableVar Environment
