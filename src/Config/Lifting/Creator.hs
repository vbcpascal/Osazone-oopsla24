{-# LANGUAGE QuasiQuotes #-}
module Config.Lifting.Creator where

-- import Text.RawString.QQ
-- import Utils.System (createAndWriteFile)

-- import Config.Core.Creator (setName, yamlFile)
-- import System.FilePath ((</>))

-- newLifting :: String -> FilePath -> IO ()
-- newLifting langName path = do
--   createAndWriteFile (setName langName $ path </> "$lang$/lifting.yaml") (sgYamlFile langName)
--   createAndWriteFile (setName langName $ path </> "$lang$/$Name$.sgs") (sgFile langName)
--   createAndWriteFile (setName langName $ path </> "$lang$/language.yaml") (yamlFile langName)

-- sgFile :: String -> String
-- sgFile langName = setName langName [r|-- Syntactic sugars of $Name$

-- TmAnd :: Term -> Term -> Term
-- TmAnd e1 e2 = TmIf e1 e2 TmFalse
-- |]

-- sgYamlFile :: String -> String
-- sgYamlFile langName = setName langName [r|# Configurations of $Name$ by lifting

-- # The name of the language after lifting
-- name:       $Name$

-- # The path of core language definiton
-- core:

-- |]
