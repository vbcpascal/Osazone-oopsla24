{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Config.Osazone.Reader
  ( readLang
  , appShowModuleDependencies
  ) where

import Config.YamlReader as Y
import Language.Osazone
import Language.Osazone.Parser.Translation (parseOsa)
import Utils.AnsiPretty
import Utils.ErrorMessage

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.State
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString as S (readFile)
import Data.Functor ((<&>))
import Data.List (insert, partition)
import Data.Map (Map, elems, empty, fromList, keys, member, toList, (!), (!?))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Yaml (ParseException, decodeThrow)
import GHC.Stack (HasCallStack)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((<.>), (</>))

-- | read a language definition from a path
readLang :: FilePath -> IO Lang
readLang path = do
  config <- readYaml path
  let name = Y.name config
  let version = Y.version config
  let extension = Y.extension config
  modules <- dsModules <$> readModules path
  return Lang
    { langName      = name
    , langVersion   = version
    , langExtension = extension
    , langModules   = (modules ! FromProject, modules ! FromLib)
    , langConfig    = config
    }

-- | read the yaml configuration from a path
readYaml :: FilePath -> IO YamlConfig
readYaml path = do
  src <- S.readFile (path </> "language.yaml")
  decodeThrow src `catch` \(e :: ParseException) -> do
    raiseErrorIO YamlParseError (viaShow e)

-- | read all related modules from the path of a language definition.
readModules :: FilePath -> IO DependencyState
readModules path = do
  config <- readYaml path
  let projPath = path </> "src/"
  let libPath = path </> lib config

  -- read all project modules and prelude modules
  let projmodQNames = map ((FromProject,, QNameI "*project") . read) (elems (Y.modules config))
  let preludes = map ((FromLib,, QNameI "*default") . read) ["Prelude", "Meta.Monad.Trans"]

  execStateT findRestDependencies $ DS
    { dsProjectPath = projPath
    , dsLibPath = libPath
    , dsParsedModule = M.fromList (initMap empty)
    , dsModules = M.fromList (initMap [])
    , dsToBeParsed = projmodQNames ++ preludes
    }
  where initMap v = map (, v) [FromProject, FromLib, FromHask]

-- ========[ Module dependency analysis ]======== --

data ModulePos
  = FromProject
  | FromLib
  | FromHask
  deriving (Ord, Eq)

data DependencyState = DS
  { dsProjectPath  :: FilePath
  , dsLibPath      :: FilePath
  , dsParsedModule :: Map ModulePos (Map QName [QName])
  , dsModules      :: Map ModulePos [Module]
  , dsToBeParsed   :: [(ModulePos, QName, QName)]  -- ^ pos, mod, imported by
  }

findRestDependencies :: HasCallStack => StateT DependencyState IO ()
findRestDependencies = do
  ds@DS{..} <- get
  case dsToBeParsed of
    [] -> return ()
    (p : ps) -> do
      put ds { dsToBeParsed = ps }
      findDependency p
      findRestDependencies

findDependency :: HasCallStack => (ModulePos, QName, QName) -> StateT DependencyState IO ()
findDependency p@(FromHask, qname, importedBy) = do
  parsed <- haveParsed p
  unless parsed (insertModule FromHask qname Nothing importedBy)
findDependency p@(_, qname, importedBy) = do
  parsed <- haveParsed p
  unless parsed $ do
    (pos, mod) <- readModuleFromQName p
    insertModule pos qname (Just mod) importedBy
    -- get dependencies
    let imports = impDecls mod <&> \case
          Import qname' _ -> (pos, qname', qname)
          ImportHS qname' _ -> (FromHask, qname', qname)
    ds@DS{..} <- get
    put (ds { dsToBeParsed = dsToBeParsed ++ imports })

-- | check whether a module has been parsed. If so, add the `importedBy` to the list
haveParsed :: HasCallStack => (ModulePos, QName, QName) -> StateT DependencyState IO Bool
haveParsed p@(FromProject, qname, importedBy) = do
  parsedProjModules <- gets ((! FromProject) . dsParsedModule)
  if qname `member` parsedProjModules
    then do { addModuleIntoImportedList p; return True }
    else haveParsed (FromLib, qname, importedBy)
haveParsed p@(pos, qname, importedBy) = do
  parsedModules <- gets ((! pos) . dsParsedModule)
  if qname `member` parsedModules
    then do { addModuleIntoImportedList p; return True }
    else return False

-- | insert the module to the parsedModules, and initialize the imported list
insertModule :: HasCallStack => ModulePos -> QName -> Maybe Module -> QName -> StateT DependencyState IO ()
insertModule pos qname (Just m) importedBy = do
  -- add the parsed module
  ds@DS{..} <- get
  let dsModules' = M.insert pos (m : dsModules ! pos) dsModules
  put ds { dsModules = dsModules' }
  insertModule pos qname Nothing importedBy
insertModule pos qname mod importedBy = do
  -- add the `importedBy` to the list
  ds@DS{..} <- get
  let parsedModules = dsParsedModule ! pos
  let parsedModules' = M.insert qname [importedBy] parsedModules
  let dsParsedModule' = M.insert pos parsedModules' dsParsedModule
  put ds { dsParsedModule = dsParsedModule' }

-- | if `A` imports `B`, add `A` into the imported list of `B`
addModuleIntoImportedList :: HasCallStack => (ModulePos, QName, QName) -> StateT DependencyState IO ()
addModuleIntoImportedList (pos, qname, importedBy) = do
  ds@DS{..} <- get
  let parsedModules = dsParsedModule ! pos
  let importedList = parsedModules ! qname
  unless (importedBy `elem` importedList) $ do
    let parsedModules' = M.insert qname (insert importedBy importedList) parsedModules
    put $ ds { dsParsedModule = M.insert pos parsedModules' dsParsedModule }

-- | read module from a QName, according to the `ModulePos`
readModuleFromQName :: HasCallStack => (ModulePos, QName, QName) -> StateT DependencyState IO (ModulePos, Module)
readModuleFromQName (pos, qname, importedBy) = do
  DS{..} <- get
  resProj <- if pos == FromProject
               then lift (searchQName dsProjectPath qname)
               else return Nothing
  resLib <- lift (searchQName dsLibPath qname)
  case (resProj, resLib) of
    (Just p, Nothing) -> lift ((,) FromProject <$> parseOsa p)
    (Nothing, Just p) -> lift ((,) FromLib <$> parseOsa p)
    (Just _, Just _) -> lift $ raiseErrorIO ModuleMultiDef $
      "module" <+> colored Blue (viaShow qname)
        <+> "imported by" <+> colored Blue (viaShow importedBy)
        <+> "has multiple definition. Found in"
        <|> indent 2 (vsep
          [ "(proj)" <+> viaShow dsProjectPath
          , "(lib) " <+> viaShow dsLibPath
          ])
    (Nothing, Nothing) -> lift $ raiseErrorIO ModuleNotFound $
      "module" <+> colored Blue (viaShow qname)
        <+> "imported by" <+> colored Blue (viaShow importedBy)
        <+> "is not found in"
        <|> indent 2 (vsep
          [ "(proj)" <+> viaShow dsProjectPath
          , "(lib) " <+> viaShow dsLibPath
          ])
-- >>> getPathFromQName "lib" (read "Meta.Identifier")
-- "lib\\Meta\\Identifier.osa"
getPathFromQName :: FilePath -> QName -> FilePath
getPathFromQName path QNameN       = path
getPathFromQName path (QNameI str) = path </> str <.> "osa"
getPathFromQName path (QNameC h t) = getPathFromQName (path </> h) (QName t)

-- | Given a path, get the real file path of the QName
searchQName :: FilePath -> QName -> IO (Maybe FilePath)
searchQName path QNameN = return Nothing
searchQName path (QNameI name) = do
  let modPath = path </> name <.> "osa"
  ex <- doesFileExist modPath
  if ex then return (Just modPath) else return Nothing
searchQName path qname@(QNameC q qs) = do
  let modPath = path </> show qname <.> "osa"
  let dir = path </> q
  fileExt <- doesFileExist modPath
  dirExt <- doesDirectoryExist dir
  if fileExt then return (Just modPath)
  else if dirExt then searchQName dir (QName qs)
  else return Nothing

appShowModuleDependencies :: FilePath -> IO ()
appShowModuleDependencies path = do
  putDocLn $ "Reading language from" <+> pretty path
    <> ". The following modules are imported." <> line
  modules <- dsParsedModule <$> readModules path
  putDocLn $ prInfo "Project" (modules ! FromProject)
  putDocLn $ prInfo "Library" (modules ! FromLib)
  putDocLn $ prInfo "Haskell" (modules ! FromHask)
  where
    prInfo name content = colored Cyan (name <+> "modules")
      <|> indent 2 (vsep (map prDeps (toList content))) <> line
    prDeps (qname, deps) = viaShow qname <+> "imported by"
      <|> indent 2 (hsep (map viaShow deps))
