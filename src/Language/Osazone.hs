module Language.Osazone (
  module Language.Osazone.AST,
  -- For module system
  imports,
  modules,
  getModule,
  getModuleMaybe,
  moduleOfIdent,
  nameOfIdent,
  qnameToPath,
  -- For definitions
  isSemantics,
  semanticsOfIdent,
  semanticsOfIdentMaybe,
  isFunction,
  funOfIdent,
  funOfIdentMaybe,
  isPureFunction,
  pureFunOfIdent,
  pureFunOfIdentMaybe,
  isMonadicFunction,
  monadicFunOfIdent,
  monadicFunOfIdentMaybe,
  -- Expressions
  isMonadic,
  matchPattern,
  varsInPattern,
  -- Annotations
  withAnnotation,
) where

import Language.Osazone.AST

import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Map (Map, empty, fromList, singleton, union, unions, (!?))
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import GHC.Stack (HasCallStack)
import System.FilePath (joinPath, (<.>))

-- | Get all the modules imported and used
imports :: Module -> [QName]
imports Module{..} = catMaybes $ impDecls <&> \case
    Import qname _ -> Just qname
    ImportHS qname _ -> Nothing

-- | Get all the modules of a `Lang`
modules :: Lang -> [Module]
modules = uncurry (++) . langModules

-- | Get module from qname (maybe)
getModuleMaybe :: Lang -> QName -> Maybe Module
getModuleMaybe lang qname =
  find (\m -> moduleName m == qname) (modules lang)

-- | Get module from qname
getModule :: HasCallStack => Lang -> QName -> Module
getModule lang qname = fromMaybe
  (error $ "Internal error: module " ++ show qname ++ " not found.\n")
  (getModuleMaybe lang qname)

-- | Get the module QName from an identifier. (use after nr)
moduleOfIdent :: QName -> QName
moduleOfIdent = QName . init . unwrapQName

nameOfIdent :: QName -> String
nameOfIdent = last . unwrapQName

-- ** Semantics

-- | Check whether an identifier is a semantics. (use after nr)
isSemantics :: Lang -> QName -> Bool
isSemantics lang = isJust . semanticsOfIdentMaybe lang

-- | Get the semantics definition of an identifier (maybe). (use after nr)
semanticsOfIdentMaybe :: HasCallStack => Lang -> QName -> Maybe SemanDef
semanticsOfIdentMaybe lang ident =
  semantics (getModule lang (moduleOfIdent ident)) !? nameOfIdent ident

-- | Get the semantics definition of an identifier. (use after nr)
semanticsOfIdent :: Lang -> QName -> SemanDef
semanticsOfIdent lang = fromJust . semanticsOfIdentMaybe lang

-- ** Functions

-- | Check whether an identifier is a function. (use after nr)
isFunction :: Lang -> QName -> Bool
isFunction _ (QName ("Haskell":_)) = True
isFunction lang qname = isPureFunction lang qname || isMonadicFunction lang qname

funOfIdentMaybe :: Lang -> QName -> Maybe FuncDef
funOfIdentMaybe lang ident = pureFunOfIdentMaybe lang ident <|> monadicFunOfIdentMaybe lang ident

funOfIdent :: Lang -> QName -> FuncDef
funOfIdent lang = fromJust . funOfIdentMaybe lang

-- | Check whether an identifier is a pure function. (use after nr)
isPureFunction :: Lang -> QName -> Bool
isPureFunction lang = isJust . pureFunOfIdentMaybe lang

pureFunOfIdentMaybe :: HasCallStack => Lang -> QName -> Maybe FuncDef
pureFunOfIdentMaybe lang ident =
  pureFunctions (getModule lang (moduleOfIdent ident)) !? nameOfIdent ident

pureFunOfIdent :: Lang -> QName -> FuncDef
pureFunOfIdent lang = fromJust . pureFunOfIdentMaybe lang

-- | Check whether an identifier is a monadic function. (use after nr)
isMonadicFunction :: Lang -> QName -> Bool
isMonadicFunction lang = isJust . monadicFunOfIdentMaybe lang

monadicFunOfIdentMaybe :: HasCallStack => Lang -> QName -> Maybe FuncDef
monadicFunOfIdentMaybe lang ident =
  monadicFunctions (getModule lang (moduleOfIdent ident)) !? nameOfIdent ident

monadicFunOfIdent :: Lang -> QName -> FuncDef
monadicFunOfIdent lang = fromJust . monadicFunOfIdentMaybe lang

-- | Check the title of an Annotation
withAnnotation :: Annotation -> String -> Bool
withAnnotation anno title | annoTitle anno == title = True
withAnnotation _ _ = False

-- | Get the path according to a module qname
qnameToPath :: QName -> FilePath
qnameToPath qname = joinPath (unwrapQName qname) <.> "hs"

-- | Check whether an expression is monadic or pure (after nameResolution)
isMonadic :: Lang -> Expr -> Bool
isMonadic _ ECon{} = False
isMonadic _ EConOp{} = False
isMonadic _ ELitNum{} = False
isMonadic _ ELitStr{} = False
isMonadic _ ETuple{} = False
isMonadic _ EList{} = False
isMonadic _ EMatch{} = True
isMonadic _ ELet{} = True
isMonadic _ EIf{} = True
isMonadic lang (EApp (e : es)) = isMonadic lang e
isMonadic _ (EVar (QName [_])) = False
isMonadic _ (EVar (QName ("Haskell" : _))) = True -- A temp behaviour
isMonadic lang (EVar qname)
  | isSemantics lang qname = True
  | isPureFunction lang qname = False
  | isMonadicFunction lang qname = True
  | otherwise = False
isMonadic _ (EVarOp (QName [_])) = False
isMonadic _ (EVarOp (QName ("Haskell" : _))) = True -- A temp behaviour
isMonadic lang (EVarOp qname) = isMonadic lang (EVar qname)
isMonadic _ _ = undefined

-- | Patterns
matchPattern :: Pattern -> Expr -> Maybe (Map String Expr)
matchPattern PWildcard _ = return empty
matchPattern (PVar x) e = return (singleton x e)
matchPattern (PTuple pats) (ETuple es) =
  unions <$> zipWithM matchPattern pats es
matchPattern (PCon qname []) (ECon qname')
  | qname `matchQName` qname' = return empty
  | otherwise = Nothing
matchPattern (PCon qname pats) (EApp (ECon qname' : es))
  | qname `matchQName` qname' = unions <$> zipWithM matchPattern pats es
  | otherwise = Nothing
matchPattern (PConOp qname []) (EConOp qname')
  | qname `matchQName` qname' = return empty
  | otherwise = Nothing
matchPattern (PConOp qname pats) (EApp (EConOp qname' : es))
  | qname `matchQName` qname' = unions <$> zipWithM matchPattern pats es
  | otherwise = Nothing
matchPattern (PConOp (QName [":"]) [pHead, pTail]) (EList (e : es)) =
  union <$> matchPattern pHead e <*> matchPattern pTail (EList es)
matchPattern _ _ = Nothing

matchQName :: QName -> QName -> Bool
matchQName qname qname' = nameOfIdent qname == nameOfIdent qname'

varsInPattern :: Pattern -> [String]
varsInPattern PWildcard       = []
varsInPattern (PVar x)        = [x]
varsInPattern (PCon _ pats)   = concatMap varsInPattern pats
varsInPattern (PConOp _ pats) = concatMap varsInPattern pats
varsInPattern (PTuple pats)   = concatMap varsInPattern pats
varsInPattern (PList pats)    = concatMap varsInPattern pats
