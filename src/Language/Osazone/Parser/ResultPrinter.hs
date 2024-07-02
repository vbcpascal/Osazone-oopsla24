module Language.Osazone.Parser.ResultPrinter
  ( showPos
  , printWithColor
  , printToken
  , module System.Console.ANSI
  ) where

import Data.List (intercalate)
import Language.Osazone.Parser.Token (Token (..))
import System.Console.ANSI
import Text.Parsec

showPos :: SourcePos -> String
showPos pos = "(" ++ show (sourceLine pos) ++ "," ++ showNum (sourceColumn pos) ++ ")"
  where showNum n | n < 10 = " " ++ show n
                  | otherwise = show n

printWithColor :: ColorIntensity -> Color -> String -> IO ()
printWithColor ci c x = do
  setSGR [SetColor Foreground ci c]
  putStrLn x
  setSGR [Reset]

printToken :: Token -> IO ()
printToken (ConId strs)        = putStrLn $ "[cid] " ++ intercalate "." strs
printToken (VarId strs)        = putStrLn $ "[vid] " ++ intercalate "." strs
printToken (ConOp strs)        = putStrLn $ "[cop] " ++ intercalate "." strs
printToken (VarOp strs)        = putStrLn $ "[vop] " ++ intercalate "." strs
printToken (LitNumber str)     = putStrLn $ "[num] " ++ str
printToken (LitString str)     = putStrLn $ "[str] " ++ str
printToken OpenParenthesis     = printWithColor Vivid Green "("
printToken CloseParenthesis    = printWithColor Vivid Green ")"
printToken OpenBrace           = printWithColor Vivid Green "{"
printToken CloseBrace          = printWithColor Vivid Green "}"
printToken OpenBracket         = printWithColor Vivid Green "["
printToken CloseBracket        = printWithColor Vivid Green "]"
printToken Semicolon           = printWithColor Vivid Green ";"
printToken Comma               = printWithColor Vivid Green ","
printToken OpenAnnotation      = printWithColor Vivid Green "[#"
printToken KwWildcard          = printWithColor Vivid Yellow "_"
printToken KwModule            = printWithColor Vivid Yellow "module"
printToken KwWhere             = printWithColor Vivid Yellow "where"
printToken KwImport            = printWithColor Vivid Yellow "import"
printToken KwType              = printWithColor Vivid Yellow "type"
printToken KwData              = printWithColor Vivid Yellow "data"
printToken KwCase              = printWithColor Vivid Yellow "case"
printToken KwOf                = printWithColor Vivid Yellow "of"
printToken KwIf                = printWithColor Vivid Yellow "if"
printToken KwThen              = printWithColor Vivid Yellow "then"
printToken KwElse              = printWithColor Vivid Yellow "else"
printToken KwFor               = printWithColor Vivid Yellow "for"
printToken KwLet               = printWithColor Vivid Yellow "let"
printToken KwIn                = printWithColor Vivid Yellow "in"
printToken KwMonad             = printWithColor Vivid Yellow "monad"
printToken KwAs                = printWithColor Vivid Yellow "as"
printToken KwOpEquals          = printWithColor Vivid Green "="
printToken KwOpPipe            = printWithColor Vivid Green "|"
printToken KwOpArrow           = printWithColor Vivid Green "->"
printToken KwOpDoubleColon     = printWithColor Vivid Green "::"
printToken KwOpLambda          = printWithColor Vivid Green "\\"
printToken KwOpDoubleColonQues = printWithColor Vivid Green "::?"
printToken KwOpDoubleDot       = printWithColor Vivid Green ".."
printToken KwFresh             = printWithColor Vivid Yellow "@fresh"
