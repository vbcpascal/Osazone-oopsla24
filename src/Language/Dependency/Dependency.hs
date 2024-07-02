{-# LANGUAGE OverloadedStrings #-}
module Language.Dependency.Dependency where

import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Map
import Prettyprinter

{-
newtype Dependency = Dependency (Map QName [QName])

instance Pretty Dependency where
  pretty (Dependency deps) = vsep $ toList deps <&> \(qname, ds) ->
    fill (maxLen + 3) ("#" <+> pretty qname <> ":") <+> hsep (L.map pretty ds)
    where maxLen = maximum $ L.map (length . show) $ keys deps
-}
