# Pretty

An implementation of Philip Wadler's [pretty printer](https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf) library in Osazone. For brevity, we choose the version in the second section, which contains fewer primitives.

Pretty offers these primitives to construct a simple document:
- `text s`, which takes the string `s` into a document.
- `line`, which is a line break.
- `nest i x`, which indents every new line of document `x` by `i` spaces.
- `x <> y`, which is the concatenation of `x` and `y`.

Furthermore, a document can have multiple possible layouts. Some line breaks can be converted to spaces, so that part of the document can be folded into a single line, which makes the printing *pretty*. Only one new primitive is offered in order to achieve this:
- `group x`, which adds a possible layout to `x`, where all of `x` itself is compressed into one line.

By concatenating documents having `group`ed layouts, parts of the whole document can be folded flexibly. At the entrance of Pretty, the primitive `pretty` is used, where `pretty w x` selects the best layout for `x`, when a line is `w` characters wide.

Wadler's pretty printer was a library in Haskell. Here, Pretty defines itself as a language extended from MiniML. The primitive functions in the article are defined as language constructs.
- The type `Doc` in the article is defined as a data type in MiniML, through the desugaring rule of `Prog`.
- The simpler primitives `nil`, `text` and `line` are desugared into constructions of values of type `Doc`.
- More involved primitives such as `nest` and `group` are defined as general recursive sugars, matching their definitions in the article, producing recursive evaluation rules.
