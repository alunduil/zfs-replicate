{-# LANGUAGE RecordWildCards #-}

module Task
  ( fromSnapshots
  , report
  )
where

import           Data.Map.Strict                ( (!)
                                                , Map
                                                , keys
                                                , mapKeys
                                                , notMember
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , remoteFileSystem
                                                )
import           Snapshot                       ( Snapshot(name) )
import           Task.Internal
import           Task.Types

fromSnapshots :: FS.FileSystem -> Map FS.FileSystem [Snapshot] -> Map FS.FileSystem [Snapshot] -> Bool -> [Task]
fromSnapshots remote localSnapshots remoteSnapshots followDelete = concat $ creates ++ destroys
 where
  remoteSnapshots' = mapKeys (dropFromName remote) remoteSnapshots
  creates =
    [ if fs `notMember` remoteSnapshots'
        then
          Task { action = Create, fileSystem = FS.remoteFileSystem remote fs, snapshot = Nothing }
            : [ Task { action = Send, fileSystem = remote, snapshot = Just s } | s <- localSnapshots ! fs ]
        else vennTasks fs
    | fs <- keys localSnapshots
    ]
  destroys =
    [ if fs `notMember` localSnapshots
        then
          [ Task { action = Destroy, fileSystem = FS.remoteFileSystem remote fs, snapshot = Just s }
            | s <- remoteSnapshots ! fs
            ]
            ++ [Task { action = Destroy, fileSystem = FS.remoteFileSystem remote fs, snapshot = Nothing }]
        else []
    | fs <- keys remoteSnapshots'
    ]
  vennTasks fs =
    (if null middles
        then [ Task { action = Destroy, fileSystem = FS.remoteFileSystem remote fs, snapshot = Just s } | s <- rights ]
        else []
      )
      ++ [ Task { action = Send, fileSystem = remote, snapshot = Just s } | s <- lefts ]
      ++ (if not (null middles) && followDelete
           then
             [ Task { action = Destroy, fileSystem = FS.remoteFileSystem remote fs, snapshot = Just s } | s <- rights ]
           else []
         )
    where (lefts, middles, rights) = venn (localSnapshots ! fs) (remoteSnapshots' ! fs)

report :: [Task] -> String
report = unlines . fmap report'
  where report' Task {..} = show action ++ " " ++ FS.name fileSystem ++ maybe "" (('@' :) . Snapshot.name) snapshot
