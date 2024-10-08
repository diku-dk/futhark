-- i8 test.
entry add = map2 i8.(+)

entry sub = map2 i8.(-)

entry mul = map2 i8.(*)

entry pow = map2 i8.(**)-- ==
-- entry: add
-- input  { [0i8, 2i8, 127i8, 127i8]
--          [0i8, 2i8, 127i8,  -2i8] }
-- output { [0i8, 4i8, -2i8,  125i8] }
-- ==
-- entry: sub
-- input  { [2i8, 0i8, 127i8]
--          [2i8, 127i8, -2i8] }
-- output { [0i8, -127i8, -127i8] }
-- ==
-- entry: mul
-- input  { [2i8, 2i8,  -2i8,  -2i8, 4i8]
--          [3i8, -3i8, 3i8, -3i8, 64i8] }
-- output { [6i8, -6i8, -6i8, 6i8, 0i8] }
-- ==
-- entry: pow
-- input  { [2i8, 2i8, 3i8, 3i8, 3i8, 3i8, 3i8, 3i8, 3i8]
--          [3i8, 0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8] }
-- output { [8i8, 1i8, 3i8, 9i8, 27i8, 81i8, -13i8, -39i8, -117i8] }
