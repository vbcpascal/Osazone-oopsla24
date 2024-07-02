[config] :eval

--* 4 + 6 * 8
Prog (Eval (ECon "Plus" [ECon "Const" [EInt 4], ECon "Times" [ECon "Const" [EInt 6], ECon "Const" [EInt 8]]]))

--* 4 * (2 + 3)
Prog (Eval (ECon "Times" [ECon "Const" [EInt 4], ECon "Plus" [ECon "Const" [EInt 2], ECon "Const" [EInt 3]]]))
