{-# LANGUAGE OverloadedStrings #-}

module Snapshot
  ( listRecursive
  )
where

import           Control                        ( die )
import           Control.Monad                  ( when )
import qualified FileSystem                    as FS
                                                ( FileSystem(name) )
import           Prelude                        ( (++)
                                                , ($)
                                                , (/=)
                                                , IO
                                                , return
                                                , unwords
                                                )
import qualified Shell                          ( process
                                                , sanitize
                                                )
import           Snapshot.Internal
import           Snapshot.Types
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( readCreateProcessWithExitCode )

listRecursive :: FS.FileSystem -> IO [Snapshot]
listRecursive fs = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  let error = Shell.sanitize stderr
  when (code /= ExitSuccess) $ die $ "error encountered while listing snapshots of " ++ FS.name fs ++ ": " ++ error
  return $ snapshots stdout
 where
  command = "/usr/bin/env - zfs list " ++ unwords options ++ " " ++ FS.name fs
  options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r", "-d 1"]
