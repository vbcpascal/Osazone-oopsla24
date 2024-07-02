[config] :e

-- A collection of birds in XML:
-- <birds>
--   <bird name="Night heron" family="Ardeidae">
--     <food>fish</food>
--   </bird>
--   <bird name="Sparrow" family="Passeridae">
--     <food>seed</food>
--     <food>insects</food>
--   </bird>
-- </birds>
-- The line below represents the XML above in this XML language. When formatted:
-- Prog (Element "birds" AttrEnd
--   (NodeCons (Element "bird" (Attr "name" "Night heron" (Attr "family" "Ardeidae" AttrEnd))
--     (NodeCons (Element "food" AttrEnd
--       (NodeCons (Text "fish") NodeEnd))
--      NodeEnd))
--   (NodeCons (Element "bird" (Attr "name" "Sparrow" (Attr "family" "Passeridae" AttrEnd))
--     (NodeCons (Element "food" AttrEnd
--       (NodeCons (Text "seed") NodeEnd))
--     (NodeCons (Element "food" AttrEnd
--       (NodeCons (Text "insects") NodeEnd))
--      NodeEnd)))
--    NodeEnd)))
-- Should evaluate to a data structure in MiniML with the types defined in `Prog` representing the XML document.

--* <birds> ... </birds>
Prog (Element (EStr "birds") AttrEnd (NodeCons (Element (EStr "bird") (Attr (EStr "name") (EStr "Night heron") (Attr (EStr "family") (EStr "Ardeidae") AttrEnd)) (NodeCons (Element (EStr "food") AttrEnd (NodeCons (Text (EStr "fish")) NodeEnd)) NodeEnd)) (NodeCons (Element (EStr "bird") (Attr (EStr "name") (EStr "Sparrow") (Attr (EStr "family") (EStr "Passeridae") AttrEnd)) (NodeCons (Element (EStr "food") AttrEnd (NodeCons (Text (EStr "seed")) NodeEnd)) (NodeCons (Element (EStr "food") AttrEnd (NodeCons (Text (EStr "insects")) NodeEnd)) NodeEnd))) NodeEnd)))
