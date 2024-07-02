module Main where

import Config.Lifting.Builder (liftLanguage)
import Config.Osazone.Dump (appDumpLangInfo)
import Config.Osazone.Reader (appShowModuleDependencies)
import Language.Osazone.Parser.Layout (appProcess)
import Language.Osazone.Parser.ModuleParser (appParseModule)
import Language.Osazone.Parser.Scanner (appScan)
import Language.Osazone.Parser.Translation (appGetASTFromFile)
import Language.Osazone.Pass.Application (testPass)
import Lifting.Parser
import Target.Haskell.Generator (generateHaskellCode)
import Target.Operational.Generator (generateOperationalSemantics)

import Config.Review (runReview)
import Control.Monad (when)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("version" : _)           -> showVersion
    ("help" : _)              -> showVersion
    ("build" : path : _)      -> generateHaskellCode path
    ("lift" : path : options) -> liftLanguage path options
    ("visualize" : path : _)  -> generateOperationalSemantics path
    ("util" : options)        -> appUtils options
    ("review" : args)         -> runReview args
    _                         -> print args

showVersion :: IO ()
showVersion = print "Osazone 0.1.3.0"

appUtils :: [String] -> IO ()
appUtils ("parse" : args) = testParse args
appUtils ("lang" : args)  = testLang args
appUtils ("pass" : strs)  = testPass strs
appUtils args             = print args

testParse :: [String] -> IO ()
testParse strs = do
  let filePath = last strs
  when ("scan" `elem` strs) (appScan filePath)
  when ("layout" `elem` strs) (appProcess filePath)
  when ("cst" `elem` strs) (appParseModule filePath)
  when ("ast" `elem` strs) (appGetASTFromFile filePath)
  when ("ext" `elem` strs) (appParseExtensionFile filePath)

testLang :: [String] -> IO ()
testLang strs = do
  let filePath = last strs
  when ("info" `elem` strs) (appDumpLangInfo filePath)
  when ("modules" `elem` strs) (appShowModuleDependencies filePath)
