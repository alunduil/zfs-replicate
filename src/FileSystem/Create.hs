module FileSystem.Create where

import           Control                        ( die )
import           Control.Monad                  ( mapM_
                                                , when
                                                )
import           Data.List                      ( inits
                                                , isSubsequenceOf
                                                )
import           FileSystem.Types
import           Prelude                 hiding ( error )
import qualified Shell                          ( process
                                                , sanitize
                                                )
import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.FilePath                ( joinPath
                                                , splitDirectories
                                                )
import           System.Process                 ( readCreateProcessWithExitCode )

create :: FileSystem -> String -> IO ()
create fs sshCommand = do
  when (null $ name fs) $ die $ "refusing to create dataset: '" ++ dataset fs ++ "'"
  fileSystems <- list topLevel sshCommand
  mapM_ create' $ filter (`notElem` (name <$> fileSystems)) $ ancestors $ name fs
  where topLevel = (fromName $ dataset fs) { readonly = readonly fs }

list :: FileSystem -> String -> IO [FileSystem]
list fs sshCommand = do
  (code, stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  let error = Shell.sanitize stderr
  when (code /= ExitSuccess) $ die $ "error encountered while listing filesystems of '" ++ name fs ++ "': " ++ error
  return $ filesystems stdout
 where
  command = sshCommand ++ "/usr/bin/env - zfs list " ++ unwords options ++ " " ++ name fs
  options = ["-H", "-o name,readonly", "-t filesystem,volume", "-r"]

ancestors :: FilePath -> [FilePath]
ancestors = tail . fmap joinPath . inits . splitDirectories

create' :: FilePath -> IO ()
create' path = do
  (code, _stdout, stderr) <- readCreateProcessWithExitCode (Shell.process command) ""
  let error = Shell.sanitize stderr
  when (code /= ExitSuccess && not ("successfully created, but not mounted" `isSubsequenceOf` error))
    $  die
    $  "unable to create remote dataset: "
    ++ path
  where command = "/usr/bin/env - zfs create -o readonly=on " ++ path

filesystems :: String -> [FileSystem]
filesystems = fmap filesystem . filter (not . null) . lines

filesystem :: String -> FileSystem
filesystem input = (fromName n) { readonly = ro == "on" } where (n, '\t' : ro) = span (/= '\t') input
