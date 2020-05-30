module Snapshot.Destroy where

import           Control                        ( die )
import           Control.Monad                  ( when )
import qualified FileSystem                    as FS
                                                ( name )
import qualified Shell                          ( process )
import           Snapshot.Types
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( readCreateProcessWithExitCode )

destroy :: Snapshot -> String -> IO ()
destroy s sshCommand = do
  (code, _stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  when (code /= ExitSuccess)
    $  die
    $  "unable to destroy snapshot: '"
    ++ FS.name (fileSystem s)
    ++ "@"
    ++ name s
    ++ "': "
    ++ stderr
  where command = sshCommand ++ " /usr/bin/env - zfs destroy '" ++ FS.name (fileSystem s) ++ "@" ++ name s ++ "'"
