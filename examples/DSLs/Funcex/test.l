[config] :e

--* true or1 false
Or1 ETrue EFalse

--* true or2 false
Or2 ETrue EFalse

--* var x = 1 in !x
NewVar "x" (EInt 1) (EDeref (EVar "x"))

--* lets x = 1; y = 2 in x + y
Lets [("x", EInt 1), ("y", EInt 2)] (EAdd (EVar "x") (EVar "y"))

--* var x = 1 in while !x < 10 do x := !x + 1 end; !x
NewVar "x" (EInt 1) (ESeq (While (ELt (EDeref (EVar "x")) (EInt 10)) (EAsgn (EVar "x") (EAdd (EDeref (EVar "x")) (EInt 1)))) (EDeref (EVar "x")))

-- Or1 is not hygienic
--* let x = true in false or1 x
ELet "x" ETrue (Or1 EFalse (EVar "x"))
-- should be false

-- Or2 is hygienic
--* let x = true in false or2 x
ELet "x" ETrue (Or2 EFalse (EVar "x"))
-- should be true
