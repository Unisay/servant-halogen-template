module Server
  ( server
  ) where

import Servant         (Server)
import Server.Api      (Api)
import Server.Handlers (execute)


server :: Server Api
server = execute
