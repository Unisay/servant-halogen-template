module Data.TcpPort where

import Preamble
import Refined

import Data.Typeable        (typeOf)
import System.Envy.Extended (Var (..))

data TcpPortRange

type TcpPort = Refined TcpPortRange Int

instance Predicate TcpPortRange Int where
  validate p port =
    unless (port >= 0 && port < 65536) $ throwRefine $ RefineOtherException
      (typeOf p)
      "Tcp port value outside of range [0, 65535]"

instance Var TcpPort where
  toVar   = show . unrefine
  fromVar = readMaybe >=> hush . refine
