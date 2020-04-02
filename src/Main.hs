{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Control                        ( die )
import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( unlessM
                                                , whenM
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , create
                                                , fromName
                                                , remoteDataset
                                                )
import           Options.Applicative
import qualified Snapshot                       ( list
                                                , ListOptions(..)
                                                , listOptions
                                                , group
                                                )
import qualified SSH                            ( Cipher(Standard)
                                                , HostName
                                                , Port
                                                , UserName
                                                , command
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesPathExist
                                                )
import qualified Task                           ( fromSnapshots
                                                , report
                                                )

data Options = Options
             { verbose :: Bool
             , followDelete :: Bool
             , recursive :: Bool
             , port :: SSH.Port
             , userName :: Maybe SSH.UserName
             , identityFile :: FilePath
             , cipher :: SSH.Cipher
             , hostName :: SSH.HostName
             , remoteFS :: FS.FileSystem
             , localFS :: FS.FileSystem
             }

main :: IO ()
main = do
  Options {..} <- execParser options

  unlessM (doesPathExist identityFile) (die ("'" <> identityFile <> "' does not exist."))
  whenM (doesDirectoryExist identityFile) (die ("'" <> identityFile <> "' is a directory."))

  let sshCommand = SSH.command cipher userName identityFile port hostName

  when verbose $ putStrLn $ "checking filesystem " ++ FS.name localFS

  localSnapshots <- Snapshot.list $ Snapshot.listOptions localFS
  -- TODO exclusions from snapshots to replicate.

  when verbose $ do
    putStrLn $ "found " ++ show (length localSnapshots) ++ " snapshots on " ++ FS.name localFS
    putStrLn ""

  let remoteFileSystem = FS.remoteDataset remoteFS localFS
  FS.create remoteFileSystem sshCommand

  when verbose $ putStrLn $ "checking filesystem " ++ hostName ++ "/" ++ FS.name remoteFileSystem

  remoteSnapshots <- Snapshot.list
    $ (Snapshot.listOptions remoteFS) { Snapshot.recursive = recursive, Snapshot.sshCommand = Just sshCommand }

  when verbose $ do
    putStrLn $ "found " ++ show (length remoteSnapshots) ++ " snapshots on " ++ FS.name remoteFileSystem
    putStrLn ""

  let tasks = Task.fromSnapshots remoteFileSystem
                                 (Snapshot.group localSnapshots)
                                 (Snapshot.group remoteSnapshots)
                                 followDelete

  when verbose $ putStrLn $ Task.report tasks

options :: ParserInfo Options
options = info (options' <**> helper) (fullDesc <> progDesc "Replicate LOCAL_FS to REMOTE_FS on HOST.")
 where
  options' =
    Options
      <$> switch (long "verbose" <> short 'v' <> help "Print additional output.")
      <*> switch (long "follow-delete" <> help "Delete snapshots on REMOTE_FS that have been deleted from LOCAL_FS.")
      <*> switch (long "recursive" <> help "Recursively replicate snapshots.")
      <*> option
            auto
            (long "port" <> short 'p' <> help "Connect to SSH on PORT." <> showDefault <> value 22 <> metavar "PORT")
      <*> option
            auto
            (  long "login"
            <> short 'l'
            <> long "user"
            <> short 'u'
            <> help "Connect to SSH as USER."
            <> value Nothing
            <> metavar "USER"
            )
      <*> strOption (long "identity-file" <> short 'i' <> help "SSH identity file to use." <> metavar "PATH")
      <*> option
            auto
            (  long "cipher"
            <> value SSH.Standard
            <> help "One of: disable (no ciphers), fast (only fast ciphers), or standard (default ciphers)."
            <> metavar "CIPHER"
            )
      <*> strArgument (metavar "HOSTNAME")
      <*> (FS.fromName <$> strArgument (metavar "REMOTE_FS"))
      <*> (FS.fromName <$> strArgument (metavar "LOCAL_FS"))
