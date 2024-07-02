# Complex

This language is taken as a MPS sample. It exhibits the complex number arithmetics. For simplicity, we assume that the real and imaginary parts are both integers.

The construct `Prog` can be seen as a DSL program entrance, which provides the declaration of new data type `Complex`. The language construct `C`, which denotes creation of complex numbers in the form `x + yi`, desugars into its constructor:
```
Prog { e } ->d
  data Complex = Complex Int Int; e
n1 + n2i ->d Complex n1 n2
```

To define syntactic sugars for arithmetic operators, we shall need to extract the real and imaginary parts of complex numbers. An auxilliary syntactic sugar `Let` (differing from `ELet` in MiniML, which binds only a variable), which desugars into a single branch pattern matching, is defined to abstract out this common pattern:
```
let p = e1 in e2 ->d case e1 of p -> e2
```
Based on this `Let`, the complex operators are defined as syntactic sugars that first extract the parts of its operands, then calculating the result using real operators. For example,
```
e1 + e2 ->d fresh x1 x2 y1 y2 in  -- Complex add
  let Complex x1 y1 = e1 in
    let Complex x2 y2 = e2 in
      Complex (x1 + x2) (y1 + y2) -- Real add
```

Details of sugars can be seen in `sugars/Complex.sgs`. Sample programs can be seen in `test.l`.
