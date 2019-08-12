module Server.Handlers
  ( execute
  ) where

import Domain

import Capability.MonadExec (MonadExec (..))
import Servant              (Handler)
import Server.Auth          (JwtPayload)

execute :: JwtPayload -> ExecTarget -> ExecConfig -> Handler ExecResult
execute _ = exec
