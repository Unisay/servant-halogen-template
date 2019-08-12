module Capability.MonadExec
  ( MonadExec (..)
  ) where

import Preamble       hiding (stderr, stdout)

import Domain
import Servant.Server (Handler)
import System.Process (readCreateProcessWithExitCode, shell)


class MonadExec m where
  exec :: ExecTarget -> ExecConfig -> m ExecResult

instance MonadExec Handler where
  exec ListProcesses _ = do
    let cmd = shell
          "ps | jq -sR '[sub(\"\n$\";\"\") \
          \| splits(\"\n\") | sub(\"^ +\";\"\") \
          \| [splits(\" +\")]] \
          \| .[0] as $header \
          \| .[1:] | [.[] | [. as $x | range($header | length) \
          \| {\"key\": $header[.], \"value\": $x[.]}] \
          \| from_entries]'"
    (_, stdout, stderr) <-
      liftIO $ readCreateProcessWithExitCode cmd ""
    return ExecResult
      { _execResultStdout = toS stdout
      , _execResultStderr = toS stderr
      }
