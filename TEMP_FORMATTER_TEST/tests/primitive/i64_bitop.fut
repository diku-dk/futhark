-- Bitwise operations on i64 values.
--
-- ==
-- entry: land
-- input { [0i64, 0i64, 0i64, 1i64, 1i64, 1i64, -1i64, -1i64, -1i64]
--         [0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64] }
-- output { [0i64, 0i64, 0i64, 0i64, 1i64, 1i64, 0i64, 1i64, -1i64] }

-- ==
-- entry: lor
-- input { [0i64, 0i64, 0i64, 1i64, 1i64, 1i64, -1i64, -1i64, -1i64, 64i64]
--         [0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 32i64]}
-- output { [0i64, 1i64, -1i64, 1i64, 1i64, -1i64, -1i64, -1i64, -1i64, 96i64] }

-- ==
-- entry: lxor
-- input { [0i64, 0i64, 0i64, 1i64, 1i64, 1i64, -1i64, -1i64, -1i64, 64i64]
--         [0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 32i64]}
-- output { [0i64, 1i64, -1i64, 1i64, 0i64, -2i64, -1i64, -2i64, 0i64, 96i64]}

-- ==
-- entry: left
-- input { [0i64, 0i64, 1i64, 1i64, -1i64, -1i64]
--         [0i64, 1i64, 0i64, 1i64, 0i64, 1i64] }
-- output { [0i64, 0i64, 1i64, 2i64, -1i64, -2i64] }

-- ==
-- entry: right
-- input { [0i64, 0i64, 1i64, 1i64, 2i64, -1i64, -1i64]
--         [0i64, 1i64, 0i64, 1i64, 1i64, 0i64, 1i64] }
-- output { [0i64, 0i64, 1i64, 0i64, 1i64, -1i64, -1i64]}

entry land = map2 (i64.&)
entry lor = map2 (i64.|)
entry lxor = map2 (i64.^)
entry left = map2 (i64.<<)
entry right = map2 (i64.>>)