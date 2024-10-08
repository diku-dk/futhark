-- Bitwise operations on i8 values.
--
-- ==
-- entry: land
-- input { [0i8, 0i8, 0i8, 1i8, 1i8, 1i8, -1i8, -1i8, -1i8]
--         [0i8, 1i8, -1i8, 0i8, 1i8, -1i8, 0i8, 1i8, -1i8] }
-- output { [0i8, 0i8, 0i8, 0i8, 1i8, 1i8, 0i8, 1i8, -1i8] }
-- ==
-- entry: lor
-- input { [0i8, 0i8, 0i8, 1i8, 1i8, 1i8, -1i8, -1i8, -1i8, 64i8]
--         [0i8, 1i8, -1i8, 0i8, 1i8, -1i8, 0i8, 1i8, -1i8, 32i8]}
-- output { [0i8, 1i8, -1i8, 1i8, 1i8, -1i8, -1i8, -1i8, -1i8, 96i8] }
-- ==
-- entry: lxor
-- input { [0i8, 0i8, 0i8, 1i8, 1i8, 1i8, -1i8, -1i8, -1i8, 64i8]
--         [0i8, 1i8, -1i8, 0i8, 1i8, -1i8, 0i8, 1i8, -1i8, 32i8]}
-- output { [0i8, 1i8, -1i8, 1i8, 0i8, -2i8, -1i8, -2i8, 0i8, 96i8]}
-- ==
-- entry: left
-- input { [0i8, 0i8, 1i8, 1i8, -1i8, -1i8]
--         [0i8, 1i8, 0i8, 1i8, 0i8, 1i8] }
-- output { [0i8, 0i8, 1i8, 2i8, -1i8, -2i8] }
-- ==
-- entry: right
-- input { [0i8, 0i8, 1i8, 1i8, 2i8, -1i8, -1i8]
--         [0i8, 1i8, 0i8, 1i8, 1i8, 0i8, 1i8] }
-- output { [0i8, 0i8, 1i8, 0i8, 1i8, -1i8, -1i8]}
entry land = map2 i8.(&)

entry lor = map2 i8.(|)

entry lxor = map2 i8.(^)

entry left = map2 i8.(<<)

entry right = map2 i8.(>>)