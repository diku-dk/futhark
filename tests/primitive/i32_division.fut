-- Test of division-like operators for i32 values.  Dividing the
-- most negative value by -1 wraps around, and dividing by zero is an
-- error.

-- ==
-- entry: divide
-- input { [7i32, -7i32, 7i32, -7i32, -2147483648i32, -2147483648i32, 2147483647i32]
--         [3i32, 3i32, -3i32, -3i32, -1i32, 1i32, -1i32] }
-- output { [2i32, -3i32, -3i32, 2i32, -2147483648i32, -2147483648i32, -2147483647i32] }
-- compiled no_webgpu input { [1i32] [0i32] } error: division by zero

-- ==
-- entry: mod
-- input { [7i32, -7i32, 7i32, -7i32, -2147483648i32, -2147483648i32, 2147483647i32]
--         [3i32, 3i32, -3i32, -3i32, -1i32, 1i32, -1i32] }
-- output { [1i32, 2i32, -2i32, -1i32, 0i32, 0i32, 0i32] }
-- compiled no_webgpu input { [1i32] [0i32] } error: division by zero

-- ==
-- entry: quot
-- input { [7i32, -7i32, 7i32, -7i32, -2147483648i32, -2147483648i32, 2147483647i32]
--         [3i32, 3i32, -3i32, -3i32, -1i32, 1i32, -1i32] }
-- output { [2i32, -2i32, -2i32, 2i32, -2147483648i32, -2147483648i32, -2147483647i32] }
-- compiled no_webgpu input { [1i32] [0i32] } error: division by zero

-- ==
-- entry: rem
-- input { [7i32, -7i32, 7i32, -7i32, -2147483648i32, -2147483648i32, 2147483647i32]
--         [3i32, 3i32, -3i32, -3i32, -1i32, 1i32, -1i32] }
-- output { [1i32, -1i32, 1i32, -1i32, 0i32, 0i32, 0i32] }
-- compiled no_webgpu input { [1i32] [0i32] } error: division by zero

entry divide = map2 (i32./)
entry mod = map2 (i32.%)
entry quot = map2 (i32.//)
entry rem = map2 (i32.%%)
