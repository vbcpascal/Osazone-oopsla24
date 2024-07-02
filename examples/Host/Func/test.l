[config] :e

--* \x. if x false true
EAbs "x" (EIf (EVar "x") EFalse ETrue)

--* (\f. \x. f x) (\x. if x false true)
EAppV (EAbs "f" (EAbs "x" (EAppV (EVar "f") (EVar "x")))) (EAbs "x" (EIf (EVar "x") EFalse ETrue))

--* let x = ref 0 in x := !x + 1; !x
ELet "x" (ERef (EInt 0)) (ESeq (EAsgn (EVar "x") (EAdd (EDeref (EVar "x")) (EInt 1))) (EDeref (EVar "x")))

--* let len = fix (\length \l. if isnil l then 0 else 1 + length (tail l)) in len [1,2,3]
ELet "len" (EFix (EAbs "length" (EAbs "l" (EIf (EIsNil (EVar "l")) (EInt 0) (EAdd (EInt 1) (EAppV (EVar "length") (ETail (EVar "l"))))) ))) (EAppV (EVar "len") (ECons (EInt 1) (ECons (EInt 2) (ECons (EInt 3) ENil))))

-- call-by-value
--* let x = ref 0 in (\y. x := 1; y) !x
ELet "x" (ERef (EInt 0)) (EAppV (EAbs "y" (ESeq (EAsgn (EVar "x") (EInt 1)) (EVar "y"))) (EDeref (EVar "x")))

-- call-by-name
--* let x = ref 0 in ((\y. x := 1; y) !x)_N
ELet "x" (ERef (EInt 0)) (EAppN (EAbs "y" (ESeq (EAsgn (EVar "x") (EInt 1)) (EVar "y"))) (EDeref (EVar "x")))
