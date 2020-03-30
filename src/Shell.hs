{-# LANGUAGE OverloadedStrings #-}

module Shell
  ( process
  , sanitize
  )
where

import           Data.Text                      ( pack
                                                , replace
                                                , strip
                                                , unpack
                                                )
import           System.Process                 ( CreateProcess(delegate_ctlc)
                                                , shell
                                                )

process :: String -> CreateProcess
process c = (shell c) { delegate_ctlc = True }

sanitize :: String -> String
sanitize = unpack . replace "WARNING: ENABLED NONE CIPHER" "" . strip . pack
