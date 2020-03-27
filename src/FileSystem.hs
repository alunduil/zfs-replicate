module FileSystem
  ( FileSystem(name)
  , fromName
  )
where

data FileSystem = FileSystem
                { dataset :: String
                , name :: String
                , readonly :: Bool
                }

fromName :: String -> FileSystem
fromName n = FileSystem { dataset = takeWhile (/= '/') n, name = n, readonly = False }
