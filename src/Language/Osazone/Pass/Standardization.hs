module Language.Osazone.Pass.Standardization where

---------------------------------------------------------------
-- This module is used to standardize a language definition. --
-- It facilities subsequent analysis and code generation.    --
-- This pass should be invoked after name resolution.        --
--                                                           --
-- Currently, this pass complete the following tasks:        --
--  - Add Identity in semantics declaration                  --
---------------------------------------------------------------

import Data.Bifunctor
import Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
import Language.Osazone
import Lifting.Extension
import Lifting.Sugar
import Utils.AnsiPretty
import Utils.ErrorMessage

standardize :: Lang -> Either ErrMsg Lang
standardize = pure . aliasType . addIdentity

-- | Add Identity monad in the inner most monad list.
addIdentity :: Lang -> Lang
addIdentity = passOnModules addIdentity'

addIdentity' :: Module -> Module
addIdentity' mod@(Module {..}) = mod
  { semantics = M.map addIdentityForSemantics semantics }

addIdentityForSemantics :: SemanDef -> SemanDef
addIdentityForSemantics seman@(SemanDef {..})
  | withInnerMonad (fst semanMonadType) = seman
  | otherwise = seman
    { semanMonadType = first (TyCon (read "Meta.Monad.Trans.Identity") :) semanMonadType }

withInnerMonad :: [Type] -> Bool
withInnerMonad []      = False
withInnerMonad (m : _) = isInnerMonad m

isInnerMonad :: Type -> Bool
isInnerMonad (TyCon qname)
  | qname == read "Meta.Monad.Trans.Identity" = True
  | qname == read "Meta.Monad.IO.IO" = True
  | otherwise = False
isInnerMonad _ = False

-- | Define monad type alias
aliasType :: Lang -> Lang
aliasType = passOnModules aliasType'

aliasType' :: Module -> Module
aliasType' mod@(Module {..}) =
  let semantics' = M.map (snd . genSynonym) semantics
      synonyms' = typeSynonym ++ mapMaybe (fst . genSynonym . snd) (M.toList semantics)
  in  mod { semantics = semantics', typeSynonym = synonyms' }

hasAlias :: SemanDef -> Bool
hasAlias SemanDef {..} = isJust (snd semanMonadType)

genSynonym :: SemanDef -> (Maybe TypeSynonym, SemanDef)
genSynonym seman@SemanDef {..} | hasAlias seman =
  let (mt, Just alias) = semanMonadType
      seman' = seman { semanMonadType = (mt, Nothing) }
      ts = TypeSynonym alias ["a"] (TyApply (foldl1 (flip TyApply) mt) (TyVar "a"))
  in  (Just ts, seman')
genSynonym seman = (Nothing, seman)

removeAlias :: SemanDef -> SemanDef
removeAlias seman@SemanDef {..} =
  seman { semanMonadType = (fst semanMonadType, Nothing) }

passOnModules :: (Module -> Module) -> Lang -> Lang
passOnModules f lang@Lang{..} = lang { langModules = bimap (map f) (map f) langModules }
