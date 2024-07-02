[config] :e

--* 2 + 3i
Prog (C 2 3)

--* (2 + 3i) + (4 - 6i)
Prog (CAdd (C 2 3) (C 4 -6))

--* (2 + 3i) - (4 - 6i)
Prog (CSub (C 2 3) (C 4 -6))

--* (2 + 3i) * (4 - 6i)
Prog (CMul (C 2 3) (C 4 -6))

--* (2 + 3i) * (5 - 4i) + (1 + i)
Prog (CAdd (CMul (C 2 3) (C 5 -4)) (C 1 1))
