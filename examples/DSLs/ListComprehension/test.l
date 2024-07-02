[config] :eval

--* let xs = [1, 2, 3] in [ x * x | x <- xs, y = x - 2, y > 0 ]
Prog (DExp (ELet "xs" (ECon "Cons" [EInt 1, ECon "Cons" [EInt 2, ECon "Cons" [EInt 3, ECon "Cons" [EInt 4, ECon "Nil" []]]]]) (For "x" (EVar "xs") (Let "y" (ESub (EVar "x") (EInt 2)) (If (ELe (EInt 0) (EVar "y")) (Yield (EMul (EVar "x") (EVar "x"))))))))