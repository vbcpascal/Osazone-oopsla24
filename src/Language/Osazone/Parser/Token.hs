module Language.Osazone.Parser.Token (Token(..)) where
import Data.List (intercalate)

data Token
  -- identifiers
  = ConId [String]
  | VarId [String]
  | ConOp [String]
  | VarOp [String]
  | LitNumber String
  | LitString String
  -- delimiters
  | OpenParenthesis
  | CloseParenthesis
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | Semicolon
  | Comma
  | OpenAnnotation
  -- keywords
  | KwWildcard
  | KwModule
  | KwWhere
  | KwImport
  | KwType
  | KwData
  | KwCase
  | KwOf
  | KwIf
  | KwThen
  | KwElse
  | KwFor
  | KwLet
  | KwIn
  | KwMonad
  | KwAs
  -- reserved operators
  | KwOpEquals
  | KwOpPipe
  | KwOpArrow
  | KwOpDoubleColon
  | KwOpLambda
  | KwOpDoubleColonQues
  | KwOpDoubleDot
  -- lifting keywords
  | KwFresh
  deriving (Eq)

instance Show Token where
  show (ConId strs)        = intercalate "." strs
  show (VarId strs)        = intercalate "." strs
  show (ConOp strs)        = intercalate "." strs
  show (VarOp strs)        = intercalate "." strs
  show (LitNumber str)     = str
  show (LitString str)     = "\"" ++ str ++ "\""
  show OpenParenthesis     = "("
  show CloseParenthesis    = ")"
  show OpenBrace           = "{"
  show CloseBrace          = "}"
  show OpenBracket         = "["
  show CloseBracket        = "]"
  show Semicolon           = ";"
  show Comma               = ","
  show OpenAnnotation      = "[#"
  show KwWildcard          = "_"
  show KwModule            = "module"
  show KwWhere             = "where"
  show KwImport            = "import"
  show KwType              = "type"
  show KwData              = "data"
  show KwCase              = "case"
  show KwOf                = "of"
  show KwIf                = "if"
  show KwThen              = "then"
  show KwElse              = "else"
  show KwFor               = "for"
  show KwLet               = "let"
  show KwIn                = "in"
  show KwMonad             = "monad"
  show KwAs                = "as"
  show KwOpEquals          = "="
  show KwOpPipe            = "|"
  show KwOpArrow           = "->"
  show KwOpDoubleColon     = "::"
  show KwOpLambda          = "\\"
  show KwOpDoubleColonQues = "::?"
  show KwOpDoubleDot       = ".."
  show KwFresh             = "@fresh"
