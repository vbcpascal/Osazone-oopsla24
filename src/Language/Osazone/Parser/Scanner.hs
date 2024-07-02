module Language.Osazone.Parser.Scanner (Token(..), SourceTok, scan, appScan) where

import Language.Osazone.Parser.Token

import Control.Monad (forM_, void)
import Data.Char
import Language.Osazone.Parser.ResultPrinter
import Text.Parsec

type P a = forall st . Parsec String st a

satCat :: GeneralCategory -> P Char
satCat cat = satisfy ((== cat) . generalCategory)

satCats :: [GeneralCategory] -> P Char
satCats cat = satisfy ((`elem` cat) . generalCategory)

singleton :: a -> [a]
singleton x = [x]

-----------------------------------------------------------
-- Literal Numbers & Strings
-----------------------------------------------------------

pLitNumber :: P Token
pLitNumber = LitNumber <$> many1 (satCat DecimalNumber)
  <?> "literal number"

pLitString :: P Token
pLitString = LitString <$> do
  strs <- between (char '"') (char '"') (many pStringChar)
  return (concat strs)
  <?> "literal string"

pStringChar :: P String
pStringChar = pStringEscape
  <|> singleton <$> satisfy (\c -> c /= '"' && c /= '\\')

-- TODO: Just a simple escape character handler
pStringEscape :: P String
pStringEscape = do
  esc <- char '\\'
  c <- satisfy (const True)
  return [esc, c]

-- pFreshVariable :: P String
-- pFreshVariable = do
--   c <- char '@'
--   ident <- pVarId
--   return (c : ident)
--   <?> "fresh variable"

-----------------------------------------------------------
-- Keywords
-----------------------------------------------------------

makeVarId :: String -> Token
makeVarId "module" = KwModule
makeVarId "where"  = KwWhere
makeVarId "import" = KwImport
makeVarId "type"   = KwType
makeVarId "data"   = KwData
makeVarId "case"   = KwCase
makeVarId "of"     = KwOf
makeVarId "if"     = KwIf
makeVarId "then"   = KwThen
makeVarId "else"   = KwElse
makeVarId "for"    = KwFor
makeVarId "let"    = KwLet
makeVarId "in"     = KwIn
makeVarId "monad"  = KwMonad
makeVarId "as"     = KwAs
makeVarId "_"      = KwWildcard
makeVarId s        = VarId [s]

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------

idCont :: P Char
idCont = oneOf "_'" <|> satCats
  [ UppercaseLetter, LowercaseLetter, TitlecaseLetter
  , DecimalNumber, LetterNumber, OtherNumber ]

pVarId :: P String
pVarId = (:) <$> (char '_' <|> satCat LowercaseLetter) <*> many idCont

pConId :: P String
pConId = (:) <$> satCats [UppercaseLetter, TitlecaseLetter] <*> many idCont

pVarId' :: P String
pVarId' = do (makeVarId -> VarId [name]) <- pVarId; pure name

pOp :: P String
pOp = many1 (oneOf "!#$%&*+./<=>?|\\^-~:@")

joinName :: String -> Maybe Token -> Token
joinName con (Just (ConId name)) = ConId (con : name)
joinName con (Just (VarId name)) = VarId (con : name)
joinName con (Just (ConOp name)) = ConOp (con : name)
joinName con (Just (VarOp name)) = VarOp (con : name)
joinName _   (Just _)            = error "impossible"
joinName con Nothing             = ConId [con]

pQIdOp :: P Token
pQIdOp = VarId . singleton <$> pVarId'
  <|> makeOp <$> pOp
  <|> joinName <$> pConId <*> optionMaybe (try (char '.' *> pQIdOp))

makeOp :: String -> Token
makeOp "->"        = KwOpArrow
makeOp "::"        = KwOpDoubleColon
makeOp "|"         = KwOpPipe
makeOp "="         = KwOpEquals
makeOp "\\"        = KwOpLambda
makeOp "::?"       = KwOpDoubleColonQues
makeOp ".."        = KwOpDoubleDot
makeOp s@(':' : _) = ConOp [s]
makeOp s           = VarOp [s]

pToken :: P Token
pToken = OpenParenthesis <$ char '('
     <|> CloseParenthesis <$ char ')'
     <|> OpenBrace <$ char '{'
     <|> CloseBrace <$ char '}'
     <|> try (OpenAnnotation <$ string "[#")
     <|> OpenBracket <$ char '['
     <|> CloseBracket <$ char ']'
     <|> Semicolon <$ char ';'
     <|> Comma <$ char ','
     <|> try (KwFresh <$ string "@fresh")
     <|> try pQIdOp
     <|> try pLitNumber
     <|> try pLitString
     <|> makeVarId <$> pVarId

-----------------------------------------------------------
-- Comments
-----------------------------------------------------------

pOneLineComment :: P ()
pOneLineComment = do
  _ <- try (string "--")
  skipMany (satisfy (/= '\n'))
  return ()

pMultiLineComment :: P ()
pMultiLineComment = do
  _ <- try (string "{-")
  manyTill anyChar (void (try (string "-}")) <|> eof)
  return ()

pWhiteSpace :: P ()
pWhiteSpace = skipMany (skipMany1 (satisfy isSpace) <|> pOneLineComment <|> pMultiLineComment)

type SourceTok = (SourcePos, Token, SourcePos)

pDocument :: P [SourceTok]
pDocument = many do
  pWhiteSpace
  p1 <- getPosition
  tok <- pToken
  p2 <- getPosition
  pWhiteSpace
  return (p1, tok, p2)

scan :: SourceName -> String -> Either ParseError [SourceTok]
scan = parse pDocument

appScan :: SourceName -> IO ()
appScan sourceName = do
  src <- readFile sourceName
  case scan sourceName src of
    Left errMsg -> putStrLn ("[test scan] error: " ++ show errMsg)
    Right tokens -> forM_ tokens $
      \(p1, tok, p2) -> do
        putStr (showPos p1 ++ " - " ++ showPos p2 ++ ": ")
        printToken tok
