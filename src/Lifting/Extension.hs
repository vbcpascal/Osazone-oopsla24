{-# LANGUAGE OverloadedStrings #-}
module Lifting.Extension
  ( module Lifting.Extension
  , module Lifting.Sugar
  , module Lifting.Filter
  ) where

import Language.Osazone
import Lifting.Filter
import Lifting.Sugar
import Utils.Pretty

data ExtensionFile = ExtensionFile
  { extFileName :: QName
  , extensions  :: [Extension]
  } deriving (Show, Eq, Ord)

data Extension
  = ExtSugars String [Sugar]
  | ExtFilters [Filter]
  | ExtRedefine QName [([Pattern], Expr)]
  deriving (Show, Eq, Ord)

instance Pretty ExtensionFile where
  pretty ExtensionFile{..} = "module" <+> viaShow extFileName <+> "where" <|> line <>
    vsep (map ((<> line) . pretty) extensions)

instance Pretty Extension where
  pretty (ExtSugars name sugars) = "sugar" <+> pretty name <+> "where"
    <|> indent 2 (vsep (map ((<> line) . pretty) sugars))
  pretty (ExtFilters filters) = "filter" <+> "where"
    <|> indent 2 (vsep (map pretty filters))
  pretty (ExtRedefine fname def) = "redefine" <+> pretty fname <+> "where"
    <|> indent 2 (pretty def)
