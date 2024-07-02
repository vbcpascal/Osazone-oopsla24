{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Files.RuntimeDotHs (runtimeDotHsPath, runtimeDotHs) where

import Text.RawString.QQ

runtimeDotHsPath :: FilePath
runtimeDotHsPath = "Runtime.hs"

runtimeDotHs :: String
runtimeDotHs = [r|{-# LANGUAGE OverloadedStrings #-}
module Runtime
( module Runtime
, module Prettyprinter
, module Prettyprinter.Render.Terminal
) where

import Prettyprinter
import Prettyprinter.Render.Terminal

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Exception

data Range
  = InnerRange
  | Range (Int, Int) (Int, Int)
  deriving (Eq, Show, Read)

type DocAnsi = Doc AnsiStyle

colored :: Color -> DocAnsi -> DocAnsi
colored = annotate . color

type RunStack = ([DocAnsi], Int)
type Runtime = StateT RunStack

pushStack :: Monad m => DocAnsi -> Runtime m ()
pushStack doc = do
  (stack, n) <- get
  put (doc : stack, n)

popStack :: Monad m => Runtime m a -> Runtime m a
popStack op = do
  res <- op
  (stack, n) <- get
  put (tail stack, n)
  return res

fresh :: Monad m => Runtime m String
fresh = do
  (stack, n) <- get
  put (stack, n + 1)
  return ("fresh_var_" ++ show n)

data RuntimeErr
  = MatchErr DocAnsi [DocAnsi]
  | UndefinedErr
  deriving (Show)

class AnsiPretty a where
  ap :: a -> DocAnsi

class ServerPrint a where
  serverPrint :: a -> DocAnsi

instance AnsiPretty RuntimeErr where
  ap (MatchErr doc docs) =
    colored Red "MatchError:" <+> "match" <+> doc <+> "failed. Expected patterns:"
      <> line <> indent 2 (hsep docs)
  ap UndefinedErr =
    colored Red "UndefinedError"

instance ServerPrint RuntimeErr where
  serverPrint :: RuntimeErr -> DocAnsi
  serverPrint (MatchErr doc docs) =
    "MatchError:" <+> "match" <+> doc <+> "failed. Expected patterns:"
      <> line <> indent 2 (hsep docs)
  serverPrint UndefinedErr = "UndefinedError"

data ErrorInfo
  = ErrorInfo RuntimeErr [DocAnsi]
  | ParseError
  deriving (Show)

instance AnsiPretty ErrorInfo where
  ap (ErrorInfo err stacks) = ap err <> line
    <> "when" <> line <> indent 2 (vsep stacks) <> line
  ap ParseError = colored Red "ParseError:" <+> "no parse." <> line

instance ServerPrint ErrorInfo where
  serverPrint (ErrorInfo err stacks) = serverPrint err <> line
    <> "Range " <> viaShow (head stacks) <> line
  serverPrint ParseError = "{ \"msg\": \"ParseError: no parse\" }" <> line

instance Exception ErrorInfo

raise :: Monad m => RuntimeErr -> Runtime m a
raise err = do
  (stack, _) <- get
  throw $ ErrorInfo err stack

matchErr :: Monad m => DocAnsi -> [DocAnsi] -> Runtime m b
matchErr e pats = raise (MatchErr e pats)
|]
