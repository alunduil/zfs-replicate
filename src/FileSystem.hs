module FileSystem
  ( FileSystem(name, readonly)
  , fromName
  , remoteDataset
  , remoteFileSystem
  , create
  )
where

import           FileSystem.Create
import           FileSystem.Types
import           Prelude                        ( ($)
                                                , (++)
                                                )

remoteDataset :: FileSystem -> FileSystem -> FileSystem
remoteDataset remote local = fromName $ name remote ++ "/" ++ dataset local

remoteFileSystem :: FileSystem -> FileSystem -> FileSystem
remoteFileSystem remote local = fromName $ name remote ++ "/" ++ name local
