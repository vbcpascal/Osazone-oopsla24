module Lifting.Lifting.Filter where

import Language.Osazone
import Lifting.Extension

import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Map (Map, fromList, member, (!))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set, difference, union)
import qualified Data.Set as S
import Debug.Trace (trace)

type ConstructSet = Map String [String]

constructFilter :: [Filter] -> Lang -> ConstructSet -> IO Lang
constructFilter flt lang cs = do
  let hostConstructs = getAllConstructs lang
  let res = S.toList (applyFilters flt cs (S.fromList []))
  let newConstructs = M.map (filter (`elem` res)) hostConstructs
  -- print newConstructs
  return lang { langModules = first (map (genModuleDefinition newConstructs)) (langModules lang) }

-- | get all the constructs in language, with the program sort
-- TODO: module name
getAllConstructs :: Lang -> Map String [String]
getAllConstructs lang =
  fromList $ typeDecl (getModule lang (QNameI "Lang")) <&>
    \DataDeclaration {..} -> (ddTypeName, map fst ddConstructors)

-- | apply a set of filters, generating the list of constructs should be preserved
applyFilters :: [Filter] -> ConstructSet -> Set String -> Set String
applyFilters [] _ curr = curr
applyFilters (f:fs) cs curr =
  let curr' = useFilter f cs curr
  in  applyFilters fs cs curr'

-- >>> useFilter (FilterUseAll "host") (fromList [("host", ["If", "True"])]) (S.fromList ["Q"])
-- fromList ["If","Q","True"]
useFilter :: Filter -> ConstructSet -> Set String -> Set String
useFilter (FilterUse s cs) _ curr   = curr `union` S.fromList cs
useFilter (FilterUseAll s) cs curr  = curr `union` S.fromList (cs ! s)
useFilter (FilterHide s cs) _ curr  = curr `difference` S.fromList cs
useFilter (FilterHideAll s) cs curr = curr `difference` S.fromList (cs ! s)

-- TODO (not implemented): users have to write all dependencies in .ext file for now
resolveDependencies :: Lang -> Map String [String]
resolveDependencies lang = undefined

genModuleDefinition :: ConstructSet -> Module -> Module
genModuleDefinition cs m@Module {..} = m
  { typeDecl = map (genDataDecl cs) typeDecl
  , semantics = M.map (genSemantic cs) semantics
  }

genDataDecl :: ConstructSet -> DataDeclaration -> DataDeclaration
genDataDecl cs dd@DataDeclaration {..}
  | ddTypeName `member` cs =
    let constructs = cs ! ddTypeName
    in dd { ddConstructors = filter ((`elem` constructs) . fst) ddConstructors }
  | otherwise = dd

genSemantic :: ConstructSet -> SemanDef -> SemanDef
genSemantic cs def@SemanDef {..} | matchRelatedType semanMatchType = def { semanDefinition = def' }
 where
  matchRelatedType (TyCon qname) = nameOfIdent qname `member` cs
  matchRelatedType _             = False
  dict = let TyCon n = semanMatchType in cs ! nameOfIdent n
  patFilter (PCon qname _) | nameOfIdent qname `elem` dict = True
  patFilter _ = False
  def' = filter (patFilter . fst) semanDefinition
genSemantic _ def = def
