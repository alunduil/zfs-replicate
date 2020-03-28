module FileSystem
  ( FileSystem(name)
  , fromName
  , remoteDataset
  )
where

data FileSystem = FileSystem
                { dataset :: String
                , name :: String
                , readonly :: Bool
                } deriving (Eq, Show)

fromName :: String -> FileSystem
fromName n = FileSystem { dataset = takeWhile (/= '/') n, name = n, readonly = False }

remoteDataset :: FileSystem -> FileSystem -> FileSystem
remoteDataset remote local = fromName $ name remote ++ "/" ++ dataset local
