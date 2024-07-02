module Language.Osazone.Parser.Layout 
  ( Tok(..)
  , process
  , appProcess
  ) where

import Language.Osazone.Parser.Scanner
import Language.Osazone.Parser.ResultPrinter 

import Control.Arrow ((&&&))
import Control.Monad (void, forM_)
import Text.Parsec.Pos (SourcePos, newPos, sourceColumn, sourceLine)

data Tok
  = Concrete SourcePos Token
  | PhantomOpenBrace
  | PhantomCloseBrace
  | PhantomSemicolon
  deriving (Eq)

data AugTok
  = Angle Int
  | Brace Int
  | Raw SourcePos Token
  deriving (Eq, Show)

mkRaw :: SourceTok -> AugTok
mkRaw (pos, tok, _) = Raw pos tok

pattern LayoutToken :: Token
pattern LayoutToken <- ((`elem` [KwOf, KwLet, KwWhere]) -> True)
  where LayoutToken = KwLet

augment :: [SourceTok] -> [AugTok]
augment (t@(_, LayoutToken, _) : rest)
  | [] <- rest = [mkRaw t, Brace 0]
  | ((sourceColumn -> c, (/= OpenBrace) -> True, _) : _) <- rest
  = mkRaw t : Brace c : augment rest
augment (t@(_, _, sourceLine -> l0) :
        rest@((sourceLine &&& sourceColumn -> (l, c), _, _) : _))
  | l0 /= l = mkRaw t : Angle c : augment rest
augment []         = []
augment (t : rest) = mkRaw t : augment rest

layout :: [AugTok] -> [Int] -> [Tok]
layout (Angle n : ts) (m : ms)
  | m == n = PhantomSemicolon : layout ts (m : ms)
  | n < m = PhantomCloseBrace : layout (Angle n : ts) ms
layout (Angle _ : ts) ms = layout ts ms
layout (Brace n : ts) (m : ms)
  | n > m = PhantomOpenBrace : layout ts (n : m : ms)
layout (Brace n : ts) [] = PhantomOpenBrace : layout ts [n]
layout (Brace n : ts) ms = PhantomOpenBrace : PhantomCloseBrace : layout (Brace n : ts) ms
layout (Raw pos CloseBrace : ts) (0 : ms) = Concrete pos CloseBrace : layout ts ms
layout (Raw _   CloseBrace : _) _ = error "see Haskell2010Report:Layout:Note3"
layout (Raw pos OpenBrace : ts) ms = Concrete pos OpenBrace : layout ts ms
layout (Raw pos t : ts) ms = Concrete pos t : layout ts ms
layout [] [] = []
layout [] (0 : _) = error "see Haskell2010Report:Layout:Note6"
layout [] (_ : ms) = PhantomCloseBrace : layout [] ms

instance Show Tok where
  show (Concrete pos token) = show pos ++ ": " ++ show token
  show PhantomOpenBrace = "{ (layout)"
  show PhantomCloseBrace = "} (layout)"
  show PhantomSemicolon = "; (layout)"

process :: [SourceTok] -> [Tok]
process = flip layout [] . augment

process' :: [SourceTok] -> [Tok]
process' = flip layout [] . tail . augment . ((nullPos, LayoutToken, nullPos) :)
  where nullPos = newPos "phantom" 0 0

appProcess :: FilePath -> IO ()
appProcess filePath = do
  src <- readFile filePath
  case scan filePath src of
    Left errMsg -> putStrLn ("[test layout] error: " ++ show errMsg)
    Right tokens -> forM_ (process tokens) printTok
  where
    printTok (Concrete pos token) = do
      putStr (showPos pos ++ ": ")
      printToken token
    printTok PhantomOpenBrace =
      printWithColor Vivid Green "{ (layout)"
    printTok PhantomCloseBrace = 
      printWithColor Vivid Green "} (layout)"
    printTok PhantomSemicolon = 
      printWithColor Vivid Green "; (layout)"
