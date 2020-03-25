module SSH
  ( command
  )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Word                      ( Word16 )
import           Prelude                        ( (++)
                                                , ($)
                                                , Eq
                                                , FilePath
                                                , Maybe(..)
                                                , Show(show)
                                                , String
                                                , null
                                                , unwords
                                                )

data Cipher = Disabled | Fast
            deriving (Eq, Show)

type HostName = String

type Port = Word16

type User = String

command :: Cipher -> User -> FilePath -> Port -> HostName -> String
command cipher user keyFile port hostName = ssh ++ " " ++ unwords (catMaybes options)
 where
  ssh = "usr/bin/env - ssh"
  options =
    toOptions cipher
      ++ [ Just $ "-i " ++ keyFile
         , Just "-o BatchMode=yes"
         , Just "-o StrictHostKeyChecking=yes"
         , Just "-o ConnectTimeout=7"
         , if null user then Nothing else Just $ "-l " ++ user
         , Just $ "-p " ++ show port
         , Just hostName
         ]

toOptions :: Cipher -> [Maybe String]
toOptions Disabled = [Just "-o noneenabled=yes", Just "-o noneswitch=yes"]
toOptions Fast     = [Just "-c arcfour256,arcfour128,blowfish-cbc,aes128-ctr,aes192-ctr,aes256-ctr"]
