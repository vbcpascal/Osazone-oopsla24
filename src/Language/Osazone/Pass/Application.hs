module Language.Osazone.Pass.Application where
import Config.Osazone.Dump (dumpLang)
import Config.Osazone.Reader (readLang)
import Control.Monad
import Language.Osazone.Parser.Translation (getASTFromFile)
import Language.Osazone.Pass (importRenamingIO, nameResolutionIO, standardizeIO)
import Language.Osazone.Pass.ImportRenaming (importRenaming)
import Language.Osazone.Pass.NameResolution (nameResolution)
import Language.Osazone.Pass.Standardization (standardize)
import System.FilePath ((</>))
import Utils.ErrorMessage (sure)
import Utils.Functions
import Utils.Pretty
import Control.Monad (when)

testPass :: [String] -> IO ()
testPass strs = do
  let filePath = last strs
  when ("ir" `elem` strs) (appImportRename filePath)
  when ("nr" `elem` strs) (appNameResolution filePath)
  when ("std" `elem` strs) (appStandardization filePath)

-- | pass ir
appImportRename :: FilePath -> IO ()
appImportRename filePath = do
  src <- readFile filePath
  mod <- sure (getASTFromFile filePath src)
  let mod' = importRenaming mod
  print (pretty mod')

-- | pass nr
appNameResolution :: FilePath -> IO ()
appNameResolution filePath = do
  lang <- readLang filePath
  lang' <- sure (nameResolution lang)
  dumpLang (filePath </> ".osazone/nr") lang'

-- | pass std
appStandardization :: FilePath -> IO ()
appStandardization filePath = do
  lang <- readLang filePath
  lang' <- (importRenamingIO >=> nameResolutionIO >=> standardizeIO) lang
  dumpLang (filePath </> ".osazone/std") lang'
