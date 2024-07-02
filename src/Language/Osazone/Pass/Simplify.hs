module Language.Osazone.Pass.Simplify where
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.Map as M
import Debug.Trace
import Language.Osazone.AST
import Language.Osazone.Pass.Simplify.AliasPropagation (aliasPropagation)
import Language.Osazone.Pass.Simplify.CaseCase
    ( flattenComp
    , ofCourseMatch
    , ofCourseMatchFail
    )
import Language.Osazone.Pass.Simplify.ConstPropagation (constPropagation)

simplifyLang :: Lang -> Lang
simplifyLang lang =
  lang { langModules = first (map (simplifyModule lang)) $ langModules lang }

simplifyModule :: Lang -> Module -> Module
simplifyModule lang mod =
  mod { semantics = M.map (simplifySemanDef lang) $ semantics mod }

simplifySemanDef :: Lang -> SemanDef -> SemanDef
simplifySemanDef lang semanDef =
  semanDef { semanDefinition = map (second (simplifyExpr lang)) (semanDefinition semanDef) }

simplifyExpr :: Lang -> Expr -> Expr
simplifyExpr lang = f . constPropagation . aliasPropagation lang . f
  where f = flattenComp . ofCourseMatch . ofCourseMatchFail . flattenComp
