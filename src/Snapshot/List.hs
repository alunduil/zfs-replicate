module Snapshot.List where

import           Control                        ( die )
import           Control.Monad                  ( when )
import           Data.Maybe                     ( catMaybes )
import qualified FileSystem                    as FS
                                                ( FileSystem(name) )
import           Prelude                        ( (++)
                                                , ($)
                                                , (/=)
                                                , Bool(False)
                                                , IO
                                                , Maybe(..)
                                                , String
                                                , maybe
                                                , return
                                                , unwords
                                                )
import           Snapshot.Internal              ( snapshots )
import           Snapshot.Types                 ( Snapshot )
import qualified Shell                          ( process
                                                , sanitize
                                                )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( readCreateProcessWithExitCode )

data ListOptions = ListOptions
                 { fileSystem :: FS.FileSystem
                 , recursive :: Bool
                 , sshCommand :: Maybe String
                 }

list :: ListOptions -> IO [Snapshot]
list ListOptions { fileSystem = fs, recursive = r, sshCommand = ssh } = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  let error = Shell.sanitize stderr
  when (code /= ExitSuccess) $ die $ "error encountered while listing snapshots of " ++ FS.name fs ++ ": " ++ error
  return $ snapshots stdout
 where
  command = maybe "" (++ " ") ssh ++ "/usr/bin/env - zfs list " ++ unwords (catMaybes options) ++ " " ++ FS.name fs
  options =
    [Just "-H", Just "-t snapshot", Just "-p", Just "-o name,creation", Just "-r", if r then Just "-d 1" else Nothing]

listOptions :: FS.FileSystem -> ListOptions
listOptions fs = ListOptions { fileSystem = fs, recursive = False, sshCommand = Nothing }
