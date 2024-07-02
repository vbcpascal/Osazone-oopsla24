[config] :e

-- This is a test file of LC language.
-- You may find some useful syntax annotation in this file.
-- Of course, you have known we use double dashes for comments
-- just like Haskell. But multi-line comments is not supported
-- due to the laziness of the author.

--* if true false true
EIf ETrue EFalse ETrue

--* \x. if x false true
EAbs "x" (EIf (EVar "x") EFalse ETrue)

--* (\x. if x false true) true
EApp (EAbs "x" (EIf (EVar "x") EFalse ETrue)) ETrue

--* (\f. \x. f x) (\x. if x false true)
EApp (EAbs "f" (EAbs "x" (EApp (EVar "f") (EVar "x")))) (EAbs "x" (EIf (EVar "x") EFalse ETrue))

--* Hello, oopsla!
Hello, oopsla!

--* true false
EApp ETrue EFalse
