{-# LANGUAGE RecordWildCards #-}

module Task
  ( Task(..)
  , fromSnapshots
  , report
  , execute
  )
where

import           Compression                    ( Compression )
import           Data.Map.Strict                ( (!)
                                                , Map
                                                , keys
                                                , mapKeys
                                                , notMember
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , create
                                                , destroy
                                                , remoteFileSystem
                                                )
import           Snapshot                       ( Snapshot(name)
                                                , destroy
                                                , send
                                                )
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

execute :: FS.FileSystem -> Map FS.FileSystem [Task] -> String -> Bool -> Compression -> IO ()
execute remote tasks sshCommand followDelete compression = mapM_ (execute' . snd) tasks'
 where
  tasks' = sortOn (length . filter (== '/') . FS.name . fst) $ assocs tasks
  execute' Task { action = Create, fileSystem = fileSystem, snapshot = Nothing }  = FS.create fileSystem sshCommand
  execute' Task { action = Destroy, fileSystem = fileSystem, snapshot = Nothing } = FS.destroy fileSystem sshCommand
  execute' Task { action = Destroy, snapshot = Just snapshot }                    = Snapshot.destroy snapshot sshCommand
  execute' Task { action = Send, snapshot = Just snapshot } =
    Snapshot.send remote snapshot sshCommand compression followDelete
