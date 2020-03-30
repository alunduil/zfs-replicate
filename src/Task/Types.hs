module Task.Types where

import qualified FileSystem                    as FS
                                                ( FileSystem )
import           Prelude                        ( Eq
                                                , Maybe
                                                , Show
                                                )
import           Snapshot                       ( Snapshot )

data Task = Task
          { action :: Action
          , fileSystem :: FS.FileSystem
          , snapshot :: Maybe Snapshot
          } deriving (Eq, Show)

data Action = Create
            | Destroy
            | Send
            deriving (Eq, Show)
