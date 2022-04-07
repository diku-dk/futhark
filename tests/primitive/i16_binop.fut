-- i16 test.

entry add = map2 (i16.+)
entry sub = map2 (i16.-)
entry mul = map2 (i16.*)
entry pow = map2 (i16.**)

-- ==
-- entry: add
-- input  { [0i16, 2i16, 32767i16, 32767i16]
--          [0i16, 2i16, 32767i16, -2i16] }
-- output { [0i16, 4i16, -2i16,    32765i16] }

-- ==
-- entry: sub
-- input  { [2i16, 0i16, 32767i16]
--          [2i16, 32767i16, -2i16] }
-- output { [0i16, -32767i16, -32767i16] }

-- ==
-- entry: mul
-- input  { [2i16, 2i16,  -2i16,  -2i16, 128i16]
--          [3i16, -3i16, 3i16, -3i16, 512i16] }
-- output { [6i16, -6i16, -6i16, 6i16, 0i16] }

-- ==
-- entry: pow
-- input  { [2i16, 2i16, 11i16, 11i16, 11i16, 11i16, 11i16, 11i16, 11i16]
--          [3i16, 0i16, 1i16, 2i16, 3i16, 4i16, 5i16, 6i16, 7i16] }
-- output { [8i16, 1i16, 11i16, 121i16, 1331i16, 14641i16, 29979i16, 2089i16, 22979i16] }