module Utils.Pretty
  ( indent'
  , putline
  , isEmpty
  , hsep'
  , vsep'
  , vsep_
  , vsep_'
  , underscore
  , arrow
  , prettyShow
  , (<|>)
  , (<+>)
  , safeList
  , safeTupled
  , module Prettyprinter
  ) where

import Prettyprinter hiding ((<+>))
import Prettyprinter.Internal (Doc (Empty))

isEmpty :: Doc ann -> Bool
isEmpty Empty = True
isEmpty _     = False

(<+>) :: Doc ann -> Doc ann -> Doc ann
(<+>) doc1 doc2 | isEmpty doc1 = doc2
                | isEmpty doc2 = doc1
                | otherwise = doc1 <> space <> doc2
infixr 6 <+>

(<|>) :: Doc ann -> Doc ann -> Doc ann
(<|>) doc1 doc2 | isEmpty doc1 = doc2
                | isEmpty doc2 = doc1
                | otherwise = doc1 <> line <> doc2
infixr 6 <|>

indent' :: Int -> Doc ann -> Doc ann
indent' _ Empty = Empty
indent' x doc   = indent x doc

putline :: Doc ann -> Doc ann
putline Empty = Empty
putline doc   = doc <> line

hsep' :: [Doc ann] -> Doc ann
hsep' = concatWith (<+>)

vsep' :: [Doc ann] -> Doc ann
vsep' docs = vsep docs <> line

vsep_ :: [Doc ann] -> Doc ann
vsep_ = concatWith (surround (pretty "\n\n"))

vsep_' :: [Doc ann] -> Doc ann
vsep_' []   = emptyDoc
vsep_' docs = vsep_ docs <> line

underscore :: Doc ann
underscore = pretty "_"

arrow :: Doc ann
arrow = pretty "->"

prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

safeTupled :: [Doc ann] -> Doc ann
safeTupled []   = pretty "()"
safeTupled docs = parens (foldr1 (\d t -> d <> pretty "," <+> t) docs)

safeList :: [Doc ann] -> Doc ann
safeList []   = pretty "[]"
safeList docs = brackets (foldr1 (\d t -> d <> pretty "," <+> t) docs)

