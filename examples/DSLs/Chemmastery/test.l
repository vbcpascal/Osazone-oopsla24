[config] :e

-- Electrolysis of water
--* 2H2O + energy --> 2H2 + O2
Prog (Equation (Plus (Compound 2 (Bond (Element H 2) (Element O 1))) Energy) "" (Plus (Compound 2 (Element H 2)) (Compound 1 (Element O 2))))

--* CaO + H2O --> Ca(OH)2
Prog (Equation (Plus (Compound 1 (Bond (Element Ca 1) (Element O 1))) (Compound 1 (Bond (Element H 2) (Element O 1)))) "" (Compound 1 (Bond (Element Ca 1) (Parens (Bond (Element O 1) (Element H 1)) 2))))

-- Photosynthesis
--*                       Chlorophyle
--* 6H2O + 6CO2 + energy -------------> C6H12O6 + 6O2
Prog (Equation (Plus (Compound 6 (Bond (Element H 2) (Element O 1))) (Plus (Compound 6 (Bond (Element C 1) (Element O 2))) Energy)) "Chlorophyle" (Plus (Compound 1 (Bond (Element C 6) (Bond (Element H 12) (Element O 6)))) (Compound 6 (Element O 2))))

-- Unbalanced example
--* C + O2 --> 2CO
Prog (Equation (Plus (Compound 1 (Element Ca 1)) (Compound 1 (Element O 2))) "" (Compound 2 (Bond (Element C 1) (Element O 1))))
