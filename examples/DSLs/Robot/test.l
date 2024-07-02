[config] :eval

--* routine turnAround means
--*   repeat 2 times
--*     turnLeft
--*   end
--* end
--*
--* repeat 5 times
--*   step
--* end
--* turnAround
--* repeat 3 times
--*   step
--* end
--* printPos
Script (DExp (Routine "turnAround" (Repeat (EInt 2) TurnLeft) (ESeq (Repeat (EInt 5) Step) (ESeq (RoutineCall "turnAround") (ESeq (Repeat (EInt 3) TurnLeft) PrintPos)))))

--* while not heading south do
--*   turnLeft
--* end
--* repeat 3 times
--*   step
--* end
--* printPos
Script (DExp (ESeq (While (Not (Heading South)) TurnLeft) (ESeq (Repeat (EInt 3) Step) PrintPos)))
