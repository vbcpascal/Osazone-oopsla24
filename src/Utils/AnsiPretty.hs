module Utils.AnsiPretty
( module Utils.AnsiPretty
, module Prettyprinter
, module Prettyprinter.Render.Terminal
) where

import Prettyprinter
import Prettyprinter.Internal (Doc (Empty))
import Prettyprinter.Render.Terminal

type DocAnsi = Doc AnsiStyle

colored :: Color -> DocAnsi -> DocAnsi
colored = annotate . color

-- Combinators

(<|>) :: DocAnsi -> DocAnsi -> DocAnsi
Empty <|> doc2  = doc2
doc1  <|> Empty = doc1
doc1  <|> doc2  = doc1 <> line <> doc2

safeTupled :: [Doc ann] -> Doc ann
safeTupled []   = pretty "()"
safeTupled docs = parens (foldr1 (\d t -> d <> pretty "," <+> t) docs)

safeList :: [Doc ann] -> Doc ann
safeList []   = pretty "[]"
safeList docs = brackets (foldr1 (\d t -> d <> pretty "," <+> t) docs)

-- Interfaces

putDocLn :: DocAnsi -> IO ()
putDocLn doc = putDoc $ doc <> line
