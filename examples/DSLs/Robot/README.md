# Robot

This is a sample language called Kaja from MPS, which controls a robot moving in a 2D grid.

In the original language, the robot has a position and a direction it faces. It could turn left, take a step in the direction it faces, and pick and drop marks in the grid. The language offers constructs to repeat a series of commands for a fixed number of times or while a condition is true, and to define routines and call them. For instance, a right turn can be simulated as three left turns:
```
routine turn right means
  repeat 3 times
    turnLeft
  end
end
```
Finally, there are commands that prints messages or set a certain grid to be wall or mark.

In this implementation, we focus on moving the robot. The position and direction of the robot are states of the language. In some other examples we choose monad extension directly, embedding the states into the evaluator. Here, we implement them as global variables of mutable reference type, where reference type acts as a more general extension. The variables `x` and `y` represents the position. The variables `dx` and `dy` represents the direction as a unit vector. Operations refer to, and alter the contents of these variables. For example, we desugar `turnLeft` to such a modification in the direction vector:
```
turnLeft ->d fresh tmp in
  let tmp = !dx in dx := 0 - dy; dy := tmp
```
Since we have sequencing of expressions in the host language, we can directly take them into the surface language. If-then-else can also be taken.

We stay away from walls or marks. Those features require a state of a 2D array, which works similarly, but makes the example unnecessarily complicated. We instead introduce a `printPos` construct that outputs where our robot is to make it more active.
