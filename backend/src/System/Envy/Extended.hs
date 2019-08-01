module System.Envy.Extended
  ( module System.Envy
  , ShowableVar(..)
  ) where

import Preamble
import System.Envy

newtype ShowableVar a
  = ShowableVar a
  deriving stock (Typeable)
  deriving newtype (Eq, Show, Read)

instance (Show a, Read a, Typeable a) => Var (ShowableVar a) where
  toVar = show
  fromVar = readMaybe
