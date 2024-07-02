# ListComprehension

This example shows how to define list comprehension through syntactic sugars.

List comprehensions are in the form
```
[ e | p, q, ... ]
```
where `p` and `q` are *qualifiers*, and `e` is the final expression. In our DSL, qualifiers can take three different forms:
- `x <- e`, which takes every `x` from the list `e`
- `e`, which proceeds with the current choices of variables only if `e` is satisfied
- `x = e`, which defines a variable depending on the values of variables already chosen.

For example, the below chooses the squares of the odd numbers from the list `xs`:
```
[ x * x | x <- xs, rem = x % 2, rem == 1 ]
```

Note that qualifiers on the right appears actually closer to the final result during evaluation. When `x` and `rem` are in scope,
```
e1 = [ x * x | rem == 1 ]
```
produces the singleton list if `rem` is `1`, and an empty list if not. Further on, when only `x` is in scope,
```
e2 = join [ e1 | rem = x % 2 ]
```
where `join` flattens a list-of-list produces a singleton list if `x` is odd, and an empty list otherwise. Finally,
```
join [ e2 | x <- xs ]
```
offers us the complete result. We could say that later qualifier groups together tighter with the final expression.

Following this line of thought, in this DSL, we define the AST of list comprehension in a list-like way, where the head of the list is the first qualifier, and the remaining qualifiers and the final expression comprise the tail. Corresponding to the three types of qualifiers, we define three language constructs:
- `For x e' e`, with the first qualifier being `x <- e'`.
- `If e' e`, with the first qualifier being `e'`.
- `Let x e' e`, with the first qualifier being `x = e'`.

The final `e` in each construct always represents the remaining qualifiers and the final expression. We further have
- `Yield e`

when there remain no qualifiers, with `e` here being the final expression. The above example could be written informally as
```
For x xs (Let rem (x % 2) (If (rem == 1) (Yield (x * x))))
```
Also following the reasoning above, each qualifier `q` should act like
```
join [ e | q ]
```
where `e` is the remaining part. Simplifying individually for each qualifier, we obtain the desugaring rules
```
For x e' e ->d concatMap (\x -> e) e'
If e' e    ->d if e' then e else []
Let x e' e ->d let x = e' in e
Yield e    ->d [e]
```
