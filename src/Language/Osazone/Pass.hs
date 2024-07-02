module Language.Osazone.Pass where

import Control.Monad ((>=>))

import Config.Osazone.Reader (readLang)
import Language.Osazone.AST
import Language.Osazone.Pass.ImportRenaming (importRenaming')
import Language.Osazone.Pass.NameResolution (nameResolution)
import Language.Osazone.Pass.Standardization (standardize)
import Utils.ErrorMessage (throwEitherIO)

readWithPass :: FilePath -> IO Lang
readWithPass = readLang >=> passIO

passIO :: Lang -> IO Lang
passIO = importRenamingIO >=> nameResolutionIO >=> standardizeIO

importRenamingIO :: Lang -> IO Lang
importRenamingIO = pure . importRenaming'

nameResolutionIO :: Lang -> IO Lang
nameResolutionIO = throwEitherIO . nameResolution

standardizeIO :: Lang -> IO Lang
standardizeIO = throwEitherIO . standardize
