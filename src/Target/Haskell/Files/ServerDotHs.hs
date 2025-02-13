{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Files.ServerDotHs (serverDotHsPath, serverDotHs) where

import Text.RawString.QQ

serverDotHsPath :: FilePath
serverDotHsPath = "Server.hs"

serverDotHs :: String
serverDotHs = [r|{-# LANGUAGE OverloadedStrings #-}
module Server where

{- [ Generated by Osazone ]
 -
 - This is the Repl module, the commands of
 - interpreter are implemented here. They are
 - defined in `language.yaml`.
 -}

$imports$

import Data.Map
import Control.Exception
import Control.Monad.Trans.State
import System.IO (hFlush, stdout)
import Runtime
import Text.Read

type Command = String
type Argument = String

handler :: ErrorInfo -> IO ()
handler e = putDoc (serverPrint e)

runCommand :: Command -> Argument -> IO ()
$cmds$
runCommand _ _ = print "Unknown command or invalid arguments"

$runcmds$

|]

