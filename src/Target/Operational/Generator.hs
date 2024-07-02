{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Target.Operational.Generator where

import Config.YamlReader
import Language.Osazone
import Utils.ErrorMessage (ErrMsg)
import Utils.Pretty
import Utils.StringTemplate (StringTemplate, newSTMP, render, (<~~))

import Config.Osazone.Reader
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader (ask), Reader, local, runReader)
import Control.Monad.State.Strict (StateT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isNumber)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Map (Map, elems, fromList, member, toList, (!))
import System.FilePath ((</>))
import Text.RawString.QQ
import Utils.System

type STMPConfig = Map String (String, String)

generateOperationalSemantics :: FilePath -> IO ()
generateOperationalSemantics path = do
  lang <- readLang path
  let p = path </> "index.html"
  createAndWriteFile p $ render $ nsText
    <~~ ("lang", langName lang)
    <~~ ("body", show (genNatural lang defaultNaturalConfig))

defaultNaturalConfig :: STMPConfig
defaultNaturalConfig = fromList
  [ ("eval", ("$lhs$ \\\\Downarrow_S $rhs$", "$lhs$ \\\\Rightarrow_S $rhs$"))
  , ("typeof", ("$lhs$ : $rhs$", "$rhs$ \\\\Leftarrow_T $lhs$"))
  ]

genNatural :: Lang -> STMPConfig -> Doc ann
genNatural lang conf = vsep (map (prettyRule conf) (genRules lang))

genRules :: Lang -> [LabelRule]
genRules lang = concat $ concat $ allSemans lang <&> \SemanDef {..} ->
  semanDefinition <&> \(pat, def) ->
    runReader (genExpr def) (BD semanName [] pat []) <&> (semanName,)
  where
    allSemans :: Lang -> [SemanDef]
    allSemans lang = concat $ fst (langModules lang) <&> elems . semantics
    target c a = EApp (ECon c : map (EVar . QName . (: [])) a)

data Judgement
  = JEval Expr Pattern
  | JEvalC Pattern Expr
  | JLet Expr Pattern
  | JGuard Expr
  | JEnv [Expr] Judgement
  deriving (Show, Eq, Ord)

data Rule = Rule [Judgement] Judgement

type LabelRule = (String, Rule)

data BuilderData = BD
  { bdSeman  :: String
  , bdJudgs  :: [Judgement]
  , bdTarget :: Pattern
  , bdEnv    :: [Expr]
  } deriving (Show, Eq, Ord)

type Builder = Reader BuilderData

genExpr :: Expr -> Builder [Rule]
genExpr (ELam vname expr) = undefined
genExpr (ELet [(p, e)] expr) = do
  j <- genJudgement p e
  local (addJudg j) (genExpr expr)
genExpr (ELet ((p, e) : bds) expr) = do
  j <- genJudgement p e
  local (addJudg j) (genExpr (ELet bds expr))
genExpr (EMatch (ETuple []) brs) = do
  rules <- forM brs $ \(Branch _ guards expr) -> do
    local (addGuards guards) (genExpr expr)
  return (concat rules)
genExpr (EMatch e brs) = do
  rules <- forM brs $ \(Branch pat guards expr) -> do
    j <- genJudgement pat e
    local (addGuards guards . addJudg j) (genExpr expr)
  return (concat rules)
genExpr e | needStep e = do
  j <- genJudgement (PVar "r") e
  local (addJudg j) (genExpr (EVar (QName ["r"])))
genExpr e = do
  bd <- ask
  let c = case bdEnv bd of
            []  -> JEvalC (bdTarget bd) e
            env -> JEnv env (JEvalC (bdTarget bd) e)
  return [Rule (bdJudgs bd) c]

needStep :: Expr -> Bool
needStep (EApp (EVar {} : _)) = True
needStep _                    = False

genJudgement :: Pattern -> Expr -> Builder Judgement
genJudgement (PTuple []) e = return (JGuard e)
genJudgement PWildcard e = return (JGuard e)
genJudgement pat e@(EApp (seman : expr : params)) = do
  bd <- ask
  if seman == EVar (QName [bdSeman bd])
    then case params of
      [] -> return $ JEval expr pat
      _  -> return $ JEnv params $ JEval (EApp [seman, expr]) pat
    else return $ JLet e pat
genJudgement pat e = return (JLet e pat)

addEnv :: String -> BuilderData -> BuilderData
addEnv x bd = bd { bdEnv = bdEnv bd ++ [EVar (QName [x])]}

addJudg :: Judgement -> BuilderData -> BuilderData
addJudg j bd = bd { bdJudgs = bdJudgs bd ++ [j] }

addGuards :: [Expr] -> BuilderData -> BuilderData
addGuards guards bd = bd { bdJudgs = bdJudgs bd ++ map JGuard guards }

type SS = StringTemplate String

prettyRule :: Map String (String, String) -> LabelRule -> Doc ann
prettyRule stmps (seman, Rule cs e)
  | seman `member` stmps =
  let ts = bimap newSTMP newSTMP (stmps ! seman) :: (SS, SS)
      docs = intersperse "\\qquad " (map (braces . prettyJudgement ts) cs)
      doc = braces (prettyJudgement ts e)
  in  "<p> \\[ \\frac{" <+> hsep docs <+> "}{" <+> doc <+> "} \\] </p>"
prettyRule _ _ = emptyDoc

-- The first one is $lhs$ => $rhs$, the second one is $lhs$ := $rhs$
prettyJudgement :: (SS, SS) -> Judgement -> Doc ann
prettyJudgement (stmp, _) (JEval e p) =
  let s = stmp
        <~~ ("lhs", nsPrettyShow e)
        <~~ ("rhs", nsPrettyShow p)
  in pretty (render s :: String)
prettyJudgement (stmp, _) (JEvalC e1 e2) =
  let s = stmp
        <~~ ("lhs", nsPrettyShow e1)
        <~~ ("rhs", nsPrettyShow e2)
  in pretty (render s :: String)
prettyJudgement (_, stmp) (JLet e p) =
  let s = stmp
        <~~ ("lhs", nsPrettyShow e)
        <~~ ("rhs", nsPrettyShow p)
  in pretty (render s :: String)
prettyJudgement _ (JGuard e) = nsPretty e
prettyJudgement (stmp, s) (JEnv es j@(JEval {})) =
  prettyJudgement (stmp <~~ ("env", show (hsep (map nsPretty es))), s) j
prettyJudgement (s, stmp) (JEnv es j@(JLet {})) =
  prettyJudgement (stmp <~~ ("env", show (hsep (map nsPretty es))), s) j
prettyJudgement _ _ = undefined

class Pretty a => NSPretty a where
  nsPretty :: a -> Doc ann

nsPrettyShow :: NSPretty a => a -> String
nsPrettyShow = show . nsPretty

nsp :: String -> String
nsp "==" = "="
nsp [c] = [c]
nsp (c:cs) | all isNumber cs = c : "_{" ++ cs ++ "}"
nsp (c:cs) = c : nsp cs
nsp s = s

instance NSPretty QName where
  nsPretty (QName qnames) = "\\mathit{" <> pretty (nsp (last qnames)) <> "}"

instance NSPretty Expr where
  nsPretty (EVar qname) = nsPretty qname
  nsPretty (ECon qname) = "\\mathit{" <> nsPretty qname <> "}"
  nsPretty (EVarOp qname) = parens $ nsPretty qname
  nsPretty (EConOp qname) = nsPretty qname
  nsPretty (EApp [EVar (QName ["subst"]), e, x, e']) =
    nsPretty e <> "[" <> nsPretty e' <> "/" <> nsPretty x <> "]"
  nsPretty (EApp es) = hsep $ intersperse "~" $ map nsPretty es
  nsPretty e = pretty e

instance NSPretty Pattern where
  nsPretty (PVar v) = pretty (nsp v)
  nsPretty (PCon qname []) = "\\mathit{" <> nsPretty qname <> "}"
  nsPretty (PCon qname patterns) =
    "\\mathit{" <> nsPretty qname <> "}~" <> hsep (intersperse "~" (map nsPretty patterns))
  nsPretty (PTuple patterns) = tupled (map nsPretty patterns)
  nsPretty PWildcard = "\\underline{~}"

nsText :: SS
nsText = newSTMP [r|<!DOCTYPE html>
<html>

<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width">
  <title>$lang$: Big-Step Operational Semantics</title>
  <script id="MathJax-script" src="https://cdn.bootcss.com/mathjax/3.0.5/es5/tex-mml-chtml.js"></script>
</head>

<body>
$body$
</body>

</html>|]
