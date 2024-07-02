[config] :eval

--* if true false true
DExp (EIf (EBool True) (EBool False) (EBool True))

--* let x = 1; x
DLet "x" (EInt 1) (DExp (EVar "x"))

--* data B = T | F; T
DData "B" [Constr "T" [], Constr "F" []] (DExp (ECon "T" []))

--* data B = T | F; case T of { T -> 1; F -> 0 }
DData "B" [Constr "T" [], Constr "F" []] (DExp (ECase (ECon "T" []) [Branch (Pattern "T" []) (EInt 1), Branch (Pattern "F" []) (EInt 0)]))

--* data B = T | F; let if = \cond tbr fbr -> case cond of { T -> tbr; F -> fbr }; if T 1 0
DData "B" [Constr "T" [], Constr "F" []] (DLet "if" (EAbs "cond" (TCon "B") (EAbs "tbr" TInt (EAbs "fbr" TInt (ECase (EVar "cond") [Branch (Pattern "T" []) (EVar "tbr"), Branch (Pattern "F" []) (EVar "fbr")])))) (DExp (EApp (EApp (EApp (EVar "if") (ECon "T" [])) (EInt 1)) (EInt 0))))
