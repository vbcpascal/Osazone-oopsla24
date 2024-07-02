[config] :e

-- Simple example
--* text "123" <> line
Prog (Layout (Concat (Text (EStr "123")) Line))

-- Example from page 3 of the article.
--* text "bbbbb[" <>
--* nest 2 (
--*   line <> text "ccc," <>
--*   line <> text "dd"
--* ) <>
--* line <> text "]"
Prog (Layout (Concat (Text (EStr "bbbbb[")) (Concat (Nest (EInt 2) (Concat Line (Concat (Text (EStr "ccc,")) (Concat Line (Text (EStr "dd")))))) (Concat Line (Text (EStr "]"))))))

-- The normal form of the above on page 4. Should produce the same results.
--* text "bbbbb[" <>
--* nest 2 line <> text "ccc," <>
--* nest 2 line <> text "dd" <>
--* nest 0 line <> text "]"
Prog (Layout (Concat (Text (EStr "bbbbb[")) (Concat (Nest (EInt 2) Line) (Concat (Text (EStr "ccc")) (Concat (Nest (EInt 2) Line) (Concat (Text (EStr "dd")) (Concat (Nest (EInt 0) Line) (Text (EStr "]")))))))))

-- The function `showTree` in the article, showing the tree
--   aaa [bbbbb [ccc, dd], eee, ffff [gg, hhh, ii]]
-- The example is manually expanded to not involve lists.
-- group (text "aaa[" <> nest 4 (
--   group (text "bbbbb[" <> nest 6 (
--     text "ccc," <> line <> text "dd"
--   ) <> text "],") <> line <>
--   text "eee," <> line <>
--   group (text "ffff[" <> nest 5 (
--     text "gg," <> line <> text "hhh," <> line <> text "ii"
--   ) <> text "]")
-- ) <> text "]")
-- Written in AST, formatted:
-- Group (Concat (Text (EStr "aaa["))
--       (Concat (Nest (EInt 4) (Concat (Group (Concat (Text (EStr "bbbbb["))
--                                             (Concat (Nest (EInt 6) (Concat (Text (EStr "ccc,"))
--                                                                    (Concat Line
--                                                                            (Text (EStr "dd")))))
--                                                     (Text (EStr "],")))))
--                              (Concat Line
--                              (Concat (Text (EStr "eee,"))
--                              (Concat Line
--                              (Concat (Group (Concat (Text (EStr "ffff["))
--                                             (Concat (Nest (EInt 5) (Concat (Text (EStr "gg,"))
--                                                                    (Concat Line
--                                                                    (Concat (Text (EStr "hhh,"))
--                                                                    (Concat Line
--                                                                            (Text (EStr "ii")))))))
--                                                     (Text (EStr "]")))))
--                                      (Text (EStr "]"))))))))
--               (Text (EStr "]"))))
Prog (Pretty (EInt 30) (Group (Concat (Text (EStr "aaa[")) (Concat (Nest (EInt 4) (Concat (Group (Concat (Text (EStr "bbbbb["))(Concat (Nest (EInt 6) (Concat (Text (EStr "ccc,")) (Concat Line (Text (EStr "dd"))))) (Text (EStr "],"))))) (Concat Line (Concat (Text (EStr "eee,")) (Concat Line (Concat (Group (Concat (Text (EStr "ffff[")) (Concat (Nest (EInt 5) (Concat (Text (EStr "gg,")) (Concat Line (Concat (Text (EStr "hhh,")) (Concat Line (Text (EStr "ii"))))))) (Text (EStr "]"))))) (Text (EStr "]")))))))) (Text (EStr "]"))))))
