{-# LANGUAGE OverloadedStrings #-}
module Lifting.Filter where

import Language.Osazone
import Utils.Pretty

data Filter
  = FilterUse String [String]
  | FilterUseAll String
  | FilterHide String [String]
  | FilterHideAll String
  deriving (Show, Eq, Ord)

instance Pretty Filter where
  pretty (FilterUse x qs) =
    "use" <+> pretty x <+> safeTupled (map pretty qs)
  pretty (FilterUseAll x) =
    "use" <+> pretty x <+> "(..)"
  pretty (FilterHide x qs) =
    "hide" <+> pretty x <+> safeTupled (map pretty qs)
  pretty (FilterHideAll x) =
    "hide" <+> pretty x <+> "(..)"
