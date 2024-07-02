# Calculator

This is a [tutorial language from MPS](https://www.jetbrains.com/help/mps/mps-calculator-language-tutorial.html), where the tutorial shows you how to build a language from ground up.

The language itself creates graphical calculator for fixed purposes. A calculator has a title, several input fields and several output fields. Values of output fields are determined from values of input fields, through formulas fixed in the Calculator program.

An example program of Calculator looks like
```
calculator volume
input width
input height
input depth

output width * height * depth
```

Originally, this DSL translates to method calls of the Swing framework. Here, we simplify it to have a text interface. It inputs values from and outputs values to the console.

The original language has four *concepts*, namely the calculator, input and output fields, and references to input fields. We define the whole calculator as a syntactic sugar that prints its title:
```
calculator title e ->d print (title ++ "\n"); e
```
The `e` here resembles the rest of the program, so we are defining the program as a recursive list. The input and output fields are defined as sugars that inputs values of variables or prints evaluated results:
```
input var e ->d let var = (print (var ++ ": "); readInt) in e
output e e' ->d print e; e'
```
Similar to MPS, the expression system is built-in: we have the host language MiniML at our disposal. We additionally have the ability to refer to variables from the host language, thus the fourth concept is not needed. Instead, we add an end-of-program sugar, that desugars simply to unit:
```
(end of program) ->d ()
```

See `sugars/Calculator.sgs` for definition of the four syntactic sugars. See `test.l` for example programs.
