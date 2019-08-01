module Main where

import Protolude

import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

import qualified ExampleSpec (run)


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  unlessM (and <$> sequence specs) exitFailure where
    specs = [ ExampleSpec.run ]
