[config] :e

--* true and false
And ETrue EFalse

--* (true or false) and (true and false)
And (Or ETrue EFalse) (And ETrue EFalse)

--* let x = false in x or false
Let "x" EFalse (Or (EVar "x") EFalse)
