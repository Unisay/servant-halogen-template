module App where

import Protolude (IO)
import System.IO (hSetEncoding, stderr, stdout, utf8)

import qualified Main


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  Main.run

