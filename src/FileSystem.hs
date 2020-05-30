module FileSystem
  ( FileSystem(name, readonly)
  , fromName
  , remoteDataset
  , remoteFileSystem
  , create
  , destroy
  )
where

import           Control                        ( die )
import           Control.Monad                  ( when )
import           FileSystem.Create
import           FileSystem.Types
import qualified Shell                          ( process )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( readCreateProcessWithExitCode )

remoteDataset :: FileSystem -> FileSystem -> FileSystem
remoteDataset remote local = fromName $ name remote ++ "/" ++ dataset local

remoteFileSystem :: FileSystem -> FileSystem -> FileSystem
remoteFileSystem remote local = fromName $ name remote ++ "/" ++ name local

destroy :: FileSystem -> String -> IO ()
destroy fs sshCommand = do
  (code, _stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  when (code /= ExitSuccess) $ die $ "unable to destroy dataset: '" ++ dataset fs ++ "': " ++ stderr
  where command = sshCommand ++ " /usr/bin/env - zfs destroy -r '" ++ name fs ++ "'"
