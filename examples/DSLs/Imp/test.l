[config] :eval

--* var x = 1; x
DExp (NewVar "x" (EInt 1) (RVar "x"))

--* var x = 1; x = x + 1; x
DExp (NewVar "x" (EInt 1) (ESeq (Asgn "x" (EPlus (RVar "x") (EInt 1))) (RVar "x")))

--* var x = 1; var y = 2; x += y; x
DExp (NewVar "x" (EInt 1) (NewVar "y" (EInt 2) (ESeq (PlusAsgn "x" (RVar "y")) (RVar "x"))))

--* var x = 1; pass
DExp (NewVar "x" (EInt 1) Pass)

--* var i = 1; var sum = 0; while (i <= 10) do { sum += i; i += 1; }; sum
DExp (NewVar "i" (EInt 1) (NewVar "sum" (EInt 0) (ESeq (While (Le (RVar "i") (EInt 10)) (ESeq (PlusAsgn "sum" (RVar "i")) (PlusAsgn "i" (EInt 1)))) (RVar "sum"))))
