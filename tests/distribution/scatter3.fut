-- From #2089.
-- ==
-- input { [[2i64,4i64],[1i64,3i64]] [[[true,false,true],[false,false,true]],[[false,true,true],[false,true,false]]] }
-- output { [[[true, true, true], [true, true, true]], [[true, true, true], [false, true, true]]] }

entry main =
  map2 (\is vs -> scatter (replicate 2 (replicate 3 true)) is vs)
