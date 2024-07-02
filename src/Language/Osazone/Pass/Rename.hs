module Language.Osazone.Pass.Rename (Renameable (..), rename, runRename) where

import Language.Osazone

import Control.Monad
import Control.Monad.State
import Data.Bifunctor (Bifunctor (first, second))
import Data.Bitraversable (bimapM)
import Data.Char (isNumber)
import Data.List (map, singleton)
import Data.Map (Map, empty, insert, member, (!), (!?))
import Data.Maybe (fromMaybe)
import Debug.Trace

rename :: Renameable a => [String] -> a -> a
rename usedVars = fst . runRename usedVars

runRename :: Renameable a => [String] -> a -> (a, [String])
runRename usedVars component =
  second used $ runState (rn component) (Renaming usedVars empty empty)

data Renaming = Renaming
  { used          :: [String]
  , subst         :: Map String String
  , varWithPrefix :: Map String [String]
  } deriving Show

class Renameable a where
  rn :: a -> State Renaming a

instance Renameable String where
  rn x = do
    renaming <- get
    return (fromMaybe x (subst renaming !? x))

instance Renameable Expr where
  rn (EVar (QName [x])) = EVar . QName . singleton <$> rn x
  rn (ELam pat e)       = ELam <$> rn pat <*> rn e
  rn (EApp es)          = EApp <$> mapM rn es
  rn (EMatch e brs)     = EMatch <$> rn e <*> mapM rn brs
  rn (ELet bindings e)  = ELet <$> mapM (bimapM rn rn) bindings <*> rn e
  rn (EIf e1 e2 e3)     = EIf <$> rn e1 <*> rn e2 <*> rn e3
  rn (ETuple es)        = ETuple <$> mapM rn es
  rn (EList es)         = EList <$> mapM rn es
  rn e                  = return e

instance Renameable Branch where
  rn (Branch pat guards e) = Branch <$> rn pat <*> mapM rn guards <*> rn e

instance Renameable Pattern where
  rn (PVar x)          = PVar <$> signNewVariable x
  rn (PCon cname pats) = PCon cname <$> mapM rn pats
  rn (PConOp cop pats) = PConOp cop <$> mapM rn pats
  rn (PTuple pats)     = PTuple <$> mapM rn pats
  rn (PList pats)      = PList <$> mapM rn pats
  rn PWildcard         = return PWildcard

-- | dispatch a new variable and update state
signNewVariable :: String -> State Renaming String
signNewVariable x = do
  Renaming {..} <- get
  let prefix = splitVariables x
  x' <- signNewVariableByPrefix prefix
  putUsedAndSubst x x'
  return x'

-- | dispatch a new variable (only prefix) without state update
signNewVariableByPrefix :: String -> State Renaming String
signNewVariableByPrefix prefix = do
  Renaming {..} <- get
  unless (prefix `member` varWithPrefix) (initial prefix)
  getNextVariableByPrefix prefix

-- | `initial "x" = ["x", "x1", "x2", "x3" ...]`
initial :: String -> State Renaming ()
initial prefix = do
  renaming <- get
  let varWithPrefix' = insert prefix (genList prefix) (varWithPrefix renaming)
  put (renaming {varWithPrefix = varWithPrefix'})
  where genList prefix = prefix : map ((prefix ++) . show) [1..]

-- | `getNextVariableByPrefix "x" = "x3"` if "x", "x1" and "x2" are used.
getNextVariableByPrefix :: String -> State Renaming String
getNextVariableByPrefix prefix = do
  renaming <- get
  let vars = dropWhile (`elem` used renaming) (varWithPrefix renaming ! prefix)
  let vwp = insert prefix (tail vars) (varWithPrefix renaming)
  put (renaming { varWithPrefix = vwp })
  return (head vars)

putUsedAndSubst :: String -> String -> State Renaming ()
putUsedAndSubst x y = do
  renaming@Renaming {..} <- get
  -- when (x `elem` used) (error (x ++ "has already been in used set."))
  put (renaming { used = y : used, subst = insert x y subst })

-- | compute "x", "x2", "x'", "x2'2" to "x"
splitVariables :: String -> String
splitVariables xs | all isSuffixChar xs = ""
splitVariables (x:xs) = x : splitVariables xs
splitVariables _ = undefined

isSuffixChar :: Char -> Bool
isSuffixChar c = isNumber c || c == '\'' || c == '_'
