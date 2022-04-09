-- i32 test.

entry add = map2 (i32.+)
entry sub = map2 (i32.-)
entry mul = map2 (i32.*)
entry pow = map2 (i32.**)

-- ==
-- entry: add
-- input  { [0i32, 2i32, 2147483647i32, 2147483647i32]
--          [0i32, 2i32, 2147483647i32, -2i32] }
-- output { [0i32, 4i32, -2i32,         2147483645i32] }

-- ==
-- entry: sub
-- input  { [2i32, 0i32, 2147483647i32]
--          [2i32, 2147483647i32, -2i32] }
-- output { [0i32, -2147483647i32, -2147483647i32] }

-- ==
-- entry: mul
-- input  { [2i32, 2i32,  -2i32,  -2i32, 262144i32]
--          [3i32, -3i32, 3i32, -3i32, 262144i32] }
-- output { [6i32, -6i32, -6i32, 6i32, 0i32] }

-- ==
-- entry: pow
-- input  { [2i32, 47i32, 47i32, 47i32, 47i32, 47i32, 47i32, 47i32, 47i32]
--          [3i32, 0i32, 1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32] }
-- output { [8i32, 1i32, 47i32, 2209i32, 103823i32, 4879681i32, 229345007i32, -2105686559i32, -183020465i32] }
