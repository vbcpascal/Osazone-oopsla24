{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.InterfaceGen where

import Config.Osazone.Reader
import Config.YamlReader as Y
import Data.Functor ((<&>))
import Data.Map (elems, toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Yaml
import Language.Osazone
import System.FilePath ((</>))
import Target.Haskell.Files.IntrpDotHs (intrpDotHs, intrpDotHsPath)
import Target.Haskell.Files.MainDotHs (mainDotHs, mainDotHsPath)
import Target.Haskell.Files.ReplDotHs (replDotHs, replDotHsPath)
import Target.Haskell.Files.RuntimeDotHs (runtimeDotHs, runtimeDotHsPath)
import Target.Haskell.Files.ServerDotHs (serverDotHs, serverDotHsPath)
import Text.RawString.QQ
import Utils.StringTemplate
import Utils.System (createAndWriteFile)

generateInterfaceFiles :: FilePath -> IO ()
generateInterfaceFiles path = do
  lang <- readLang path
  let targetPath = path </> "build/"
  writeMain targetPath lang
  writeRepl targetPath lang
  writeIntrp targetPath lang
  createAndWriteFile (targetPath </> runtimeDotHsPath) runtimeDotHs

writeMain :: FilePath -> Lang -> IO ()
writeMain path lang = do
  createAndWriteFile (path </> mainDotHsPath)
    (putInSTMP (langConfig lang) mainDotHs)

writeRepl :: FilePath -> Lang -> IO ()
writeRepl path lang = do
  let config = langConfig lang
      cmds :: [String]
      cmds = toList (repl config) <&> \(cmd, _) -> render $
        newSTMP cmdstmp
          <~~ ("cmd", cmd)
      runcmds :: [String]
      runcmds = toList (repl config) <&> \(cmd, def) -> render $
        newSTMP runcmdstmp
          <~~ ("cmd", cmd)
          <~~ ("def", render (newSTMP def <~~ ("input", "e")) :: String)
      imports :: [String]
      imports = elems (Y.modules config) ++ fromMaybe [] (Y.extra config) <&>
        \m -> render $ newSTMP "import Lib.$file$\n\n" <~~ ("file", m)
      str :: String
      str = render (newSTMP replDotHs
        <~~ ("cmds", cmds)
        <~~ ("runcmds", runcmds)
        <~~ ("imports", imports)
        <~~ ("name", langName lang))
  createAndWriteFile (path </> replDotHsPath) str

writeIntrp :: FilePath -> Lang -> IO ()
writeIntrp path lang = do
  let config = langConfig lang
      cmds :: [String]
      cmds = toList (repl config) <&> \(cmd, _) -> render $
        newSTMP "(\":$cmd$\", run$cmd$) : " <~~ ("cmd", cmd)
  let str = render (newSTMP intrpDotHs <~~ ("cmds", cmds))
  createAndWriteFile (path </> intrpDotHsPath) str

cmdstmp :: String
cmdstmp = [r|runCommand cmd str | cmd == ":$cmd$" = catch (run$cmd$ str) handler

|]

runcmdstmp :: String
runcmdstmp = [r|run$cmd$ :: String -> IO ()
run$cmd$ str = case readMaybe str of
  Just e -> print ($def$)
  Nothing -> throw ParseError


|]
