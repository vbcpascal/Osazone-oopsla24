[config] :e

--* : SQUARE DUP * ; 3 SQUARE 4 SQUARE + .
DExp (Let "SQUARE" (Seq Dup (Seq Mul End)) (Seq (Lit 3) (Seq (Call "SQUARE") (Seq (Lit 4) (Seq (Call "SQUARE") (Seq Plus Dot))))))
