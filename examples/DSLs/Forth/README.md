# Forth

This is an excerpt from the stack-based language Forth, which works on
a global stack, with most of the language constructs manipulating its topmost elements.

Consider the following Forth program.
```Forth
3 DUP * 4 DUP * + .
```
Literals `3` and `4` pushes themselves onto the stack. `DUP` duplicates the element
on top. Binary operators work by popping two elements off the stack as its arguments, and then
pushing the result back. This program thus first multiplies two `3`'s, and then multiplies two `4`'s, before adding them up. Finally, the sum `25` is output by the `.` operator. As seen, Forth programs executes each operation sequentially.

Users can also define their own operations between a `:` and a `;`:
```Forth
: square DUP * ;
```
This defines an operation named `square`. When performed, it sequentially performs `DUP` and `*`. We could use this definition to simplify our first program:
```Forth
3 square 4 square + .
```
which runs just identically.

The implementation of Forth can be divided into two steps.

First, we have to support the global stack as a (new language feature) via monad extension. We define it as a state monad over MiniML. Two meta-functions `push` and `pop` are introduced to manipulate the stack state. Also, `PUSH` and `POP` are two language constructs inserted to invoke the meta functions. Other language constructs and their evaluation rules of MiniML stay untouched. This extended language "MiniML+Stack" now manages the stack as mutable global state and provides two primitive operations on this stack. Based on this, we can define operations of Forth as syntactic sugars. For example,
```
DUP ->d let x = POP in PUSH x; PUSH x
```
Sugar definitions of other constructs can be seen in `Forth.sgs`.
