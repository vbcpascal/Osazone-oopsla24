{-# LANGUAGE OverloadedStrings #-}
module Config.Osazone.Dump where

import Language.Osazone.AST
import Utils.AnsiPretty
import Utils.StringTemplate
import Utils.System

import Config.Osazone.Reader (readLang)
import Data.List (intercalate)
import Data.Yaml (encode)
import System.FilePath (joinPath, (<.>), (</>))
import Text.RawString.QQ

dumpLang :: FilePath -> Lang -> IO ()
dumpLang path Lang{..} = do
  createAndWriteFileB (path </> "language.yaml") (encode langConfig)
  mapM_ (dumpModule (path </> "src/")) (fst langModules)

dumpLangInfo :: Lang -> IO ()
dumpLangInfo Lang{..} = do
  putDocLn $ "Language" <+> colored Yellow (pretty langName) <> ":"
  putDocLn $ indent 2 $ vsep
    [ "version:" <+> pretty langVersion
    , "extension:" <+> pretty langExtension
    ]
  putDocLn $ "modules:" <|> indent 2 (vsep (map (viaShow . moduleName) (fst langModules)))
  putDocLn $ "modules from lib:" <|> indent 2 (vsep (map (viaShow . moduleName) (snd langModules)))

dumpModule :: FilePath -> Module -> IO ()
dumpModule path mod =
  let QName qname = moduleName mod
      path' = joinPath (path : qname) <.> "osa"
  in  createAndWriteFile path' (show (pretty mod))

-- ^ lang info
appDumpLangInfo :: FilePath -> IO ()
appDumpLangInfo path = do
  lang <- readLang path
  dumpLangInfo lang
