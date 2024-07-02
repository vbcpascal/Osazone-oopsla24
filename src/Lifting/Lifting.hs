module Lifting.Lifting where

import Config.YamlReader (LiftingConfig)
import Language.Osazone
import Lifting.Extension
import Lifting.Lifting.SugarLifting (liftSugar)

import Control.Monad (foldM)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Map (Map, empty, fromList, insert, singleton, union, (!))
import Lifting.Lifting.Filter

-- lifting :: LiftingConfig -> ExtensionFile -> Lang -> Lang
-- lifting config extfile core = foldl (flip liftExtension) core (extensions extfile)

liftingIO :: LiftingConfig -> ExtensionFile -> Lang -> IO Lang
liftingIO config extfile core = do
  putStrLn $ "Lifting extensions in " ++ show (extFileName extfile)
  fst <$> foldM (flip liftExtension)
                (core, singleton "host" hostConstructs)
                (extensions extfile)
 where
  hostConstructs = concatMap (\DataDeclaration {..} -> map fst ddConstructors)
                             (typeDecl (getModule core (QNameI "Lang")))

liftExtension :: Extension -> (Lang, ConstructSet) -> IO (Lang, ConstructSet)
liftExtension (ExtSugars name sugars) (lang, s) = do
  putStrLn $ "- Lifting sugars defined in " ++ name
  lang' <- foldM (\l s -> return (liftSugar s l)) lang sugars
  return (lang', insert name (map sgName sugars) s)
liftExtension (ExtFilters flts) (lang, s) = do
  putStrLn "- Running the filter"
  lang' <- constructFilter flts lang s
  return (lang', s)
liftExtension (ExtRedefine qname defs) (lang, s) = do
  putStrLn $ "- Replacing the definition of " ++ show qname
  let lang' = replaceDefinition qname defs lang
  return (lang', s)

replaceDefinition :: QName -> [([Pattern], Expr)] -> Lang -> Lang
replaceDefinition qname defs lang = lang { langModules = first walk (langModules lang) }
 where
  fname = nameOfIdent qname
  mname = moduleOfIdent qname
  walk (m : ms) | moduleName m == mname = work m : ms
                | otherwise = m : walk ms
  work mod | isMonadicFunction lang qname =
    let fdef = (monadicFunctions mod ! fname) { funcDefinition = defs }
    in  mod { monadicFunctions = insert fname fdef (monadicFunctions mod) }
  work mod =
    let fdef = (pureFunctions mod ! fname) { funcDefinition = defs }
    in  mod { pureFunctions = insert fname fdef (pureFunctions mod) }
