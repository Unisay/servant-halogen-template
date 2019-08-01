module Server
  ( server
  ) where

import Servant

import Server.Api      (UserApi)
import Server.Handlers (users)


server :: Server UserApi
server = users
