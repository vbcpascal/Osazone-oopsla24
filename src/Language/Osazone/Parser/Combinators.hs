module Language.Osazone.Parser.Combinators where

import Language.Osazone.Parser.Scanner
import Language.Osazone.Parser.Layout

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Data.Functor (void)

import Text.Parsec hiding (satisfy)

type P a = forall st . Parsec [Tok] st a

parseMaybe :: (Tok -> Maybe a) -> Parsec [Tok] st a
parseMaybe = tokenPrim show nextPos
  where nextPos _ (Concrete p _) _ = p
        nextPos p _              _ = p

satisfy :: (Tok -> Bool) -> Parsec [Tok] st Tok
satisfy p = parseMaybe \x -> if p x then Just x else Nothing

satisfy_ :: (Tok -> Bool) -> Parsec [Tok] st ()
satisfy_ = void . satisfy

exact :: Token -> P Token
exact t = parseMaybe \case Concrete _ t' | t == t' -> Just t; _ -> Nothing

phantom :: Token -> P ()
phantom OpenBrace  = satisfy_ (== PhantomOpenBrace)
phantom CloseBrace = satisfy_ (== PhantomCloseBrace)
phantom Semicolon  = satisfy_ (== PhantomSemicolon)
phantom _          = error "This token can never be phantom!"

braces :: P a -> P a
braces p = exact OpenBrace *> p <* exact CloseBrace
       <|> phantom OpenBrace *> p <* phantom CloseBrace

semicolon :: P ()
semicolon = void (exact Semicolon) <|> phantom Semicolon

braceList :: P a -> P [a]
braceList p = braces (p `sepBy` semicolon)

parens :: P a -> P a
parens p = exact OpenParenthesis *> p <* exact CloseParenthesis

brackets :: P a -> P a
brackets p = exact OpenBracket *> p <* exact CloseBracket

varId :: P [String]
varId = parseMaybe \case (Concrete _ (VarId s)) -> Just s; _ -> Nothing

conId :: P [String]
conId = parseMaybe \case (Concrete _ (ConId s)) -> Just s; _ -> Nothing

varOp :: P [String]
varOp = parseMaybe \case (Concrete _ (VarOp s)) -> Just s; _ -> Nothing

conOp :: P [String]
conOp = parseMaybe \case (Concrete _ (ConOp s)) -> Just s; _ -> Nothing

litNum :: P String
litNum = parseMaybe \case (Concrete _ (LitNumber num)) -> Just num; _ -> Nothing

litStr :: P String
litStr = parseMaybe \case (Concrete _ (LitString str)) -> Just str; _ -> Nothing

notQualified :: P [String] -> P String
notQualified name = do [x] <- name; pure x

sepBy1' :: P a -> P b -> P [a]
sepBy1' x sep = (:) <$> x <*> many (try (sep *> x))

header :: String -> ParsecT s u m a -> ParsecT s u m a
header = flip label
