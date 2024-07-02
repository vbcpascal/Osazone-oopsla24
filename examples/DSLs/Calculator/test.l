[config] :eval

-- calculator volume
-- input width
-- input height
-- input depth
-- output width * height * depth
Calculator (EStr "volume") (Input "width" (Input "height" (Input "depth" (Output (EMul (EVar "width") (EMul (EVar "height") (EVar "depth"))) End))))

-- calculator mySalary
-- input Java Hours
-- input PHP Hours
-- output "Java Hours" * 10 + "PHP Hours" * 5
Calculator (EStr "mySalary") (Input "Java Hours" (Input "PHP Hours" (Output (EPlus (EMul (EVar "Java Hours") (EInt 10)) (EMul (EVar "PHP Hours") (EInt 5))) End)))
