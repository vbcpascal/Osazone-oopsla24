{-# LANGUAGE OverloadedStrings #-}
module Config.Review (runReview) where

import Config.Lifting.Builder (liftLanguage)
import Control.Monad (forM_, when)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , removeDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>))
import Target.Haskell.Generator (generateHaskellCode)
import Utils.AnsiPretty

runReview :: [String] -> IO ()
runReview args = case args of
  ("crazy-build": args') -> build args'
  ("clean": args')       -> clean args'
  _ -> putStrLn $ "unknown arguments of review: " ++ show args

build :: [String] -> IO ()
build ("--dsl" : _)  = buildDSL
build ("--host" : _) = buildHost
build []             = do { buildHost; putStrLn "\n"; buildDSL }
build args           = putStrLn $ "unknown arguments of review crazy-build: " ++ show args

buildHost :: IO ()
buildHost = do
  putDocLn "Build all the host languages\n"
  let total = length hostLanguageLists
  forM_ (zip hostLanguageLists ([1..] :: [Int])) $ \(path, id) -> do
    putDocLn $ colored Magenta $
      "[" <+> pretty id <+> "of" <+> pretty total <+> "] host" <+> pretty path
    generateHaskellCode path
    putDocLn ""

buildDSL :: IO ()
buildDSL = do
  putDocLn "Build all the DSLs\n"
  let total = length dslLists
  forM_ (zip dslLists ([1..] :: [Int])) $ \(path, id) -> do
    putDocLn $ colored Cyan $
      "[" <+> pretty id <+> "of" <+> pretty total <+> "] DSL" <+> pretty path
    liftLanguage path []
    putDocLn ""
    generateHaskellCode path
    putDocLn ""

clean :: [String] -> IO ()
clean ("--dsl" : _)  = cleanDSL
clean ("--host" : _) = cleanHost
clean []             = do { cleanHost; putStrLn "\n"; cleanDSL }
clean args           = putStrLn $ "unknown arguments of clean: " ++ show args

cleanHost :: IO ()
cleanHost = do
  putStrLn "Cleaning all the generated files for Hosts\n"
  putStrLn "[ build/ ] Path"
  putStrLn "---------------"
  forM_ (zip hostLanguageLists [1..]) $ \(path, id) -> do
    putStr "[ "
    let buildPath = path </> "build"
    doesDirectoryExist buildPath `thenDo` removeDirectoryRecursive buildPath
    putStrLn $ "] " ++ path

cleanDSL :: IO ()
cleanDSL = do
  putStrLn "Cleaning all the generated files for DSLs\n"
  putStrLn "[ language.yaml  src/  build/ ] Path"
  putStrLn "------------------------------------"
  forM_ (zip dslLists [1..]) $ \(path, id) -> do
    putStr "[ "
    let yamlPath = path </> "language.yaml"
    let srcPath = path </> "src"
    let buildPath = path </> "build"
    doesFileExist yamlPath `thenDo` removeFile yamlPath
    doesDirectoryExist srcPath `thenDo` removeDirectoryRecursive srcPath
    doesDirectoryExist buildPath `thenDo` removeDirectoryRecursive buildPath
    putStrLn $ "] " ++ path

thenDo :: IO Bool -> IO () -> IO ()
thenDo cond f = do { c <- cond; if c then do { putStr "√ "; f } else putStr "x " }

hostLanguageLists :: [FilePath]
hostLanguageLists =
  [ "examples/Host/Func"
  , "examples/Host/LC"
  , "examples/Host/MiniML"
  , "examples/Host/MiniMLRef"
  , "examples/Host/MiniMLStack"
  ]

dslLists :: [FilePath]
dslLists =
  [ "examples/DSLs/Bool"
  , "examples/DSLs/Calculator"
  , "examples/DSLs/Chemmastery"
  , "examples/DSLs/Complex"
  , "examples/DSLs/CrossModelGeneration"
  , "examples/DSLs/Deep"
  , "examples/DSLs/Forth"
  , "examples/DSLs/Funcex"
  , "examples/DSLs/Imp"
  , "examples/DSLs/ListComprehension"
  , "examples/DSLs/Pretty"
  , "examples/DSLs/Robot"
  , "examples/DSLs/Shallow"
  , "examples/DSLs/StateMachine"
  , "examples/DSLs/XML"
  ]
