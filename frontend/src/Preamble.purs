module Preamble 
  ( module Prelude
  , module DM
  , module DS
  , S
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as DM 
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol) as DS

type S = DS.SProxy