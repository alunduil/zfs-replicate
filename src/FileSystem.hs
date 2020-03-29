module FileSystem
  ( FileSystem(name)
  , fromName
  , remoteDataset
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
