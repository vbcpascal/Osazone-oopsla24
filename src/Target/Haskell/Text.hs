{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Text (writeTargetFile) where

import Config.YamlReader
import Data.Map as M
import Data.Maybe as MB
import Text.RawString.QQ

import Data.Functor ((<&>))
import Target.Haskell.Files.MetaFile (idDotHs, substDotHs)
import Target.Haskell.Files.MetaMonadFile
    ( envDotHs
    , errDotHs
    , ioDotHs
    , stateDotHs
    , transDotHs
    )
import Utils.StringTemplate
import Utils.System (createAndWriteFile)

writeTargetFile :: FilePath -> YamlConfig -> IO ()
writeTargetFile path config = undefined

{- do
  writeTF mainDotHs
  mapM_ writeLibs
    [ substDotHs, idDotHs
    , transDotHs, errDotHs, envDotHs, stateDotHs, ioDotHs ]
  writeTFRepl replDotHs
  writeTFIntrp intrpDotHs
  where
    writeTF :: (FilePath, String) -> IO ()
    writeTF (fpath, file) =
      createAndWriteFile (path ++ fpath) $ putInSTMP config file
    writeLibs :: (FilePath, String) -> IO ()
    writeLibs (fpath, file) =
      createAndWriteFile (path ++ fpath) file
    writeTFRepl :: (FilePath, String) -> IO ()
    writeTFRepl _ | MB.isNothing (haskcfg config) =
      putStrLn "Error: empty command settings for Haskell in language.yaml"
    writeTFRepl (fpath, file) = do
      let cmds :: [String]
          cmds = M.toList (fromJust (haskcfg config)) <&> \(cmd, _) -> render $
            newSTMP "run cmd str | cmd == \":$cmd$\" = run$cmd$ str\n\n"
              <~~ ("cmd", cmd)
          runcmds :: [String]
          runcmds = M.toList (fromJust (haskcfg config)) <&> \(cmd, def) -> render $
            newSTMP "run$cmd$ :: String -> String\nrun$cmd$ str = show ($def$)\n\n\n"
              <~~ ("cmd", cmd)
              <~~ ("def", render (newSTMP def <~~ ("input", "(read str)")) :: String)
      let str = render (newSTMP file <~~ ("cmds", cmds) <~~ ("runcmds", runcmds))
      writeTF (fpath, str)
    writeTFIntrp :: (FilePath, String) -> IO ()
    writeTFIntrp (fpath, file) = do
      let cmds :: [String]
          cmds = M.toList (fromJust (haskcfg config)) <&> \(cmd, _) -> render $
            newSTMP "(\":$cmd$\", run$cmd$) : " <~~ ("cmd", cmd)
      let str = render (newSTMP file <~~ ("cmds", cmds))
      writeTF (fpath, str)
-}

mainDotHs :: (FilePath, String)
mainDotHs = ("Main.hs", f)
  where f = [r|module Main where
{- [ Generated by Osazone ]
 -
 - This is the Main module, as the entrance
 - of the interpreter. Compile this file with
 - GHC as `ghc Main.hs` and you will get an
 - interpreter named Main.exe.
 -}

import Lang
import Repl
import Intrp

import System.Environment

main :: IO ()
main = do
  putStrLn "$Name$ interpreter: generated by Osazone-0.1.0.1"
  args <- getArgs
  if null args
    then repl
    else do
      f <- readFile (head args)
      intrp (lines f)

|]

replDotHs :: (FilePath, String)
replDotHs = ("Repl.hs", f)
  where f = [r|{-# LANGUAGE OverloadedStrings #-}
module Repl where
{- [ Generated by Osazone ]
 -
 - This is the Repl module, the commands of
 - interpreter are implemented here. They are
 - defined in `package.yaml`.
 -}

import Lang
import Data.Map
import Control.Exception
import System.IO (hFlush, stdout)

import Meta.Monad.Trans
import Meta.Monad.State
import Meta.Monad.Err
import Meta.Monad.Env
import Meta.Monad.Io

type Command = String
type Argument = String

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == "" then repl else do
    let (cmd:args) = words line
    let arg = unwords args
    case cmd of
      ":q" -> return ()
      "--" -> repl
      _ -> do
        catch (putStrLn (run cmd arg)) handler
        repl

handler :: PatternMatchFail -> IO ()
handler _ = putStrLn "error"

run :: Command -> Argument -> String
$cmds$
run _ _ = "Unknown command or invalid arguments"

$runcmds$

|]

intrpDotHs :: (FilePath, String)
intrpDotHs = ("Intrp.hs", f)
  where f = [r|module Intrp (intrp) where

import Repl

import Data.List
import Data.Map
import Data.String
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

cmdMap :: Map String (String -> String)
cmdMap = fromList ($cmds$[])

data IntrpSt = IntrpSt
  { current :: Int
  , behaviors :: [String]
  , label :: Maybe String
  }

type S = StateT IntrpSt IO

intrp :: [String] -> IO ()
intrp ls = evalStateT (mapM f ls >> pure ()) (IntrpSt 1 [] Nothing)
  where f l = intrpLine l >> incCurr

intrpLine :: String -> S ()
intrpLine l | isPrefixOf "--*" l = do
  let lbl = unwords (tail (words l))
  st <- get
  put (st { label = Just lbl })
intrpLine l | isPrefixOf "--" l = return ()
intrpLine l | isPrefixOf "[config] " l = do
  let rs = tail (words l)
  mapM_ checkr rs
  st <- get
  put (st { behaviors = rs })
intrpLine l | Data.List.null (words l) = return ()
intrpLine l = do
  st <- get
  lbl <- getLabel l
  lift (putStrLn ("==== Line " ++ show (current st) ++ ": " ++ lbl))
  mapM (intrpItem l) (behaviors st)
  removeLabel

intrpItem :: String -> String -> S ()
intrpItem l r = do
  lbl <- getLabel l
  let f = cmdMap ! r
  lift (putStrLn ("[" ++ r ++ "] " ++ f l))

incCurr :: S ()
incCurr = do
  st <- get
  put (st { current = current st + 1 })

getLabel :: String -> S String
getLabel l = do
  st <- get
  case label st of
    Nothing -> return l
    Just lbl -> return lbl

removeLabel :: S ()
removeLabel = do
  st <- get
  put (st { label = Nothing })

checkr :: String -> S ()
checkr c | member c cmdMap = return ()
checkr c = do
  st <- get
  error ("Unknown command " ++ c ++ " in configuration in line " ++ show (current st) ++ ".")

|]
