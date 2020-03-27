{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( (<=<)
                                                , unlessM
                                                , whenM
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , fromName
                                                )
import           Options.Applicative
import           Prelude
import qualified SSH                            ( Cipher(Standard)
                                                , HostName
                                                , Port
                                                , UserName
                                                , command
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesPathExist
                                                )
import           System.Exit                    ( exitFailure )

data Options = Options
             { verbose :: Bool
             , port :: SSH.Port
             , userName :: Maybe SSH.UserName
             , identityFile :: FilePath
             , cipher :: SSH.Cipher
             , hostName :: SSH.HostName
             , remote :: FS.FileSystem
             , local :: FS.FileSystem
             }

main :: IO ()
main = do
  Options {..} <- execParser options

  unlessM (doesPathExist identityFile) (die ("'" <> identityFile <> "' does not exist."))
  whenM (doesDirectoryExist identityFile) (die ("'" <> identityFile <> "' is a directory."))

  let sshCommand = SSH.command cipher userName identityFile port hostName

  when verbose $ putStrLn $ "checking filesystem " ++ FS.name local

  putStrLn sshCommand

  return ()


options :: ParserInfo Options
options = info (options' <**> helper) (fullDesc <> progDesc "Replicate LOCAL_FS to REMOTE_FS on HOST.")
 where
  options' =
    Options
      <$> switch (long "verbose" <> short 'v' <> help "Print additional output.")
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

die :: String -> IO a
die = const exitFailure <=< putStrLn
