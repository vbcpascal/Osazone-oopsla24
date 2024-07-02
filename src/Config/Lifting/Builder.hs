module Config.Lifting.Builder where

import Config.Osazone.Dump (dumpLang)
import Config.Osazone.Reader (readLang)
import Config.YamlReader
import Language.Osazone
import Language.Osazone.Pass (readWithPass)
import Language.Osazone.Pass.NameResolution (nameResolutionForExtension)
import Language.Osazone.Pass.Simplify (simplifyLang)
import Lifting.Extension
import Lifting.Lifting
import Lifting.Parser (parseExtensionFile)
import Utils.ErrorMessage
import Utils.Pretty (pretty, viaShow)
import Utils.StringTemplate (PreSTMP (putInSTMP))
import Utils.System (createAndWriteFile)

import Control.Monad (foldM, when)
import Control.Monad.Catch
import qualified Data.ByteString as S
import Data.Yaml
import System.FilePath (addTrailingPathSeparator, (<.>), (</>))
import Target.Haskell.Generator (generateHaskellCode)

liftLanguage :: FilePath -> [String] -> IO ()
liftLanguage path options = do
  (config, exts) <- readLifting path
  core <- readCore config path
  extfiles <- mapM (nameResolve core) exts
  lang <- foldM (flip (liftingIO config)) core extfiles
  let renamed = renameLang (surfName config) lang
      libRelocated = relocateLib (corePath config) renamed
  putStrLn "- Simplifying the generated language"
  dumpLang (addTrailingPathSeparator path) (simplifyLang (simplifyLang libRelocated))
  putStrLn "Done."

  when ("--build" `elem` options) $ do
    putStrLn ""
    generateHaskellCode path
 where nameResolve core ext = sure (nameResolutionForExtension core ext)

renameLang :: String -> Lang -> Lang
renameLang name lang = lang { langName = name, langConfig = (langConfig lang) { name = name } }

relocateLib :: FilePath -> Lang -> Lang
relocateLib corePath lang = let config = langConfig lang
                                libPath = corePath </> lib config
                        in lang { langConfig = config { lib = libPath } }

readLifting :: FilePath -> IO (LiftingConfig, [ExtensionFile])
readLifting path = do
  config <- readYaml path
  exts <- readExt config path
  return (config, exts)

readExt :: LiftingConfig -> FilePath -> IO [ExtensionFile]
readExt config path = do
  let names = extFiles config
  mapM (readExtFile path) names
 where
  readExtFile path name = do
    let f = path </> "sugars" </> name <.> "ext"
    src <- readFile f
    case parseExtensionFile f src of
      Left e     -> raiseErrorIO ExtParseError (viaShow e)
      Right file -> return file

readYaml :: FilePath -> IO LiftingConfig
readYaml path = do
  src <- S.readFile (path </> "lifting.yaml")
  decodeThrow src

readCore :: LiftingConfig -> FilePath -> IO Lang
readCore config path = do
  let core = path </> corePath config
  readWithPass core
