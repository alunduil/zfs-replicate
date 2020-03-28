{-# LANGUAGE OverloadedStrings #-}

module Snapshot
  ( listRecursive
  )
where

import           Control                        ( die )
import           Control.Monad                  ( when )
import           Data.Text                      ( pack
                                                , replace
                                                , strip
                                                , unpack
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name) )
import           Prelude                        ( (++)
                                                , ($)
                                                , (/=)
                                                , Bool(True)
                                                , IO
                                                , return
                                                , unwords
                                                )
import           Snapshot.Internal
import           Snapshot.Types
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( delegate_ctlc
                                                , readCreateProcessWithExitCode
                                                , shell
                                                )

listRecursive :: FS.FileSystem -> IO [Snapshot]
listRecursive fs = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode process ""
  let error = unpack $ replace "WARNING: ENABLED NONE CIPHER" "" $ strip $ pack stderr
  when (code /= ExitSuccess) $ die $ "error encountered while listing snapshots of " ++ FS.name fs ++ ": " ++ error
  return $ snapshots stdout
 where
  process = (shell command) { delegate_ctlc = True }
  command = "/usr/bin/env - zfs list " ++ unwords options ++ " " ++ FS.name fs
  options = ["-H", "-t snapshot", "-p", "-o name,creation", "-r", "-d 1"]
