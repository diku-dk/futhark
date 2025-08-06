-- Bitwise operations on i16 values.
--
-- ==
-- entry: land
-- input { [0i16, 0i16, 0i16, 1i16, 1i16, 1i16, -1i16, -1i16, -1i16]
--         [0i16, 1i16, -1i16, 0i16, 1i16, -1i16, 0i16, 1i16, -1i16] }
-- output { [0i16, 0i16, 0i16, 0i16, 1i16, 1i16, 0i16, 1i16, -1i16] }

-- ==
-- entry: lor
-- input { [0i16, 0i16, 0i16, 1i16, 1i16, 1i16, -1i16, -1i16, -1i16, 64i16]
--         [0i16, 1i16, -1i16, 0i16, 1i16, -1i16, 0i16, 1i16, -1i16, 32i16]}
-- output { [0i16, 1i16, -1i16, 1i16, 1i16, -1i16, -1i16, -1i16, -1i16, 96i16] }

-- ==
-- entry: lxor
-- input { [0i16, 0i16, 0i16, 1i16, 1i16, 1i16, -1i16, -1i16, -1i16, 64i16]
--         [0i16, 1i16, -1i16, 0i16, 1i16, -1i16, 0i16, 1i16, -1i16, 32i16]}
-- output { [0i16, 1i16, -1i16, 1i16, 0i16, -2i16, -1i16, -2i16, 0i16, 96i16]}

-- ==
-- entry: left
-- input { [0i16, 0i16, 1i16, 1i16, -1i16, -1i16]
--         [0i16, 1i16, 0i16, 1i16, 0i16, 1i16] }
-- output { [0i16, 0i16, 1i16, 2i16, -1i16, -2i16] }

-- ==
-- entry: right
-- input { [0i16, 0i16, 1i16, 1i16, 2i16, -1i16, -1i16]
--         [0i16, 1i16, 0i16, 1i16, 1i16, 0i16, 1i16] }
-- output { [0i16, 0i16, 1i16, 0i16, 1i16, -1i16, -1i16]}

entry land = map2 (i16.&)
entry lor = map2 (i16.|)
entry lxor = map2 (i16.^)
entry left = map2 (i16.<<)
entry right = map2 (i16.>>)
