module Snapshot.Send where

send :: FS.FileSystem -> Snapshot -> String -> Compression -> Bool -> IO ()
send remote s sshCommand c followDelete = do
  where command = send' s followDelete ++ " | " ++ receive

send' :: Snapshot -> Bool -> String
send' s followDelete =
  "/usr/bin/env - zfs send " ++ unwords (catMaybes options) ++ " '" ++ FS.name (fileSystem s) ++ "@" ++ name s ++ "'"
 where
  options =
    [ if followDelete then Just "-p" else Nothing
    , if isNothing (previous s)
      then Nothing
      else Just ("-i " ++ FS.name (filesystem $ previous s) ++ "@" ++ name (previous s) ++ "'")
    ]

compress :: Compression -> String
compress Compression.LZ4 = undefined
