module Language.Dependency.Resolver where

import Data.Functor
import Data.List (map)
import Data.Map (Map)
import qualified Data.Map
import Data.Set

{- 
resolveDependencies :: Lang -> Map QName [QName]
resolveDependencies lang = Data.Map.map toList $
  Data.Map.unionsWith union deps
  where deps = Data.Map.elems (semantics lang)
               <&> \(SemanDef _ _ _ defs) -> Data.Map.map dependencies defs

dependencies :: Expr -> Set QName
dependencies (ECon (QName ("HS":_))) = empty
dependencies (ECon qname)      = singleton qname
dependencies (EConOp (QName ("HS":_))) = empty
dependencies (EConOp qname)    = singleton qname
dependencies (ELam _ expr)     = dependencies expr
dependencies (EApp exprs)      = mconcat (Data.List.map dependencies exprs)
dependencies (EMatch expr brs) =
  let exQs  = dependencies expr
      brsQs = Data.List.map depBranch brs
  in  mconcat (exQs:brsQs)
dependencies (ELet bindings expr) = mconcat (dependencies expr:bdQs)
  where bdQs = bindings <&> \(pat, expr') -> depPattern pat `union` dependencies expr'
dependencies (ETuple exprs) = mconcat (Data.List.map dependencies exprs)
dependencies (EList exprs) = mconcat (Data.List.map dependencies exprs)
dependencies _ = empty

depBranch :: Branch -> Set QName
depBranch (Branch pat guards expr) =
  let patQs = depPattern pat
      gdsQs = Data.List.map dependencies guards
      exQs  = dependencies expr
  in  mconcat (patQs:exQs:gdsQs)

depPattern :: Pattern -> Set QName
depPattern (PCon qname pats) = insert qname (mconcat (Data.List.map depPattern pats))
depPattern _ = empty
-}