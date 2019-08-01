-- | This module leverages the PureScript foreign function interface (FFI) 
-- | to import a popular NPM library Marked, 
-- | to parse a markdown string into HTML. 
module App.Foreign.Marked 
  ( RawHTML
  , marked
  ) where

newtype RawHTML = RawHTML String

foreign import markedImpl :: String -> String

marked :: String -> RawHTML
marked str = RawHTML (markedImpl str)