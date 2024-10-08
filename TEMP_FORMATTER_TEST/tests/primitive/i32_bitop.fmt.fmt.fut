-- Bitwise operations on i32 values.
--
-- ==
-- entry: land
-- input { [0i32, 0i32, 0i32, 1i32, 1i32, 1i32, -1i32, -1i32, -1i32]
--         [0i32, 1i32, -1i32, 0i32, 1i32, -1i32, 0i32, 1i32, -1i32] }
-- output { [0i32, 0i32, 0i32, 0i32, 1i32, 1i32, 0i32, 1i32, -1i32] }
-- ==
-- entry: lor
-- input { [0i32, 0i32, 0i32, 1i32, 1i32, 1i32, -1i32, -1i32, -1i32, 64i32]
--         [0i32, 1i32, -1i32, 0i32, 1i32, -1i32, 0i32, 1i32, -1i32, 32i32]}
-- output { [0i32, 1i32, -1i32, 1i32, 1i32, -1i32, -1i32, -1i32, -1i32, 96i32] }
-- ==
-- entry: lxor
-- input { [0i32, 0i32, 0i32, 1i32, 1i32, 1i32, -1i32, -1i32, -1i32, 64i32]
--         [0i32, 1i32, -1i32, 0i32, 1i32, -1i32, 0i32, 1i32, -1i32, 32i32]}
-- output { [0i32, 1i32, -1i32, 1i32, 0i32, -2i32, -1i32, -2i32, 0i32, 96i32]}
-- ==
-- entry: left
-- input { [0i32, 0i32, 1i32, 1i32, -1i32, -1i32]
--         [0i32, 1i32, 0i32, 1i32, 0i32, 1i32] }
-- output { [0i32, 0i32, 1i32, 2i32, -1i32, -2i32] }
-- ==
-- entry: right
-- input { [0i32, 0i32, 1i32, 1i32, 2i32, -1i32, -1i32]
--         [0i32, 1i32, 0i32, 1i32, 1i32, 0i32, 1i32] }
-- output { [0i32, 0i32, 1i32, 0i32, 1i32, -1i32, -1i32]}
entry land = map2 i32.(&)

entry lor = map2 i32.(|)

entry lxor = map2 i32.(^)

entry left = map2 i32.(<<)

entry right = map2 i32.(>>)