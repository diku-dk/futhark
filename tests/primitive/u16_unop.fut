-- Test unary operators for u16.

-- ==
-- entry: negateu16
-- input { [0u16, 1u16, 65535u16, 8u16, 65528u16] }
-- output { [0u16, 65535u16, 1u16, 65528u16, 8u16] }

-- ==
-- entry: absu16
-- input { [0u16, 1u16, 65535u16, 8u16, 65528u16] }
-- output { [0u16, 1u16, 65535u16, 8u16, 65528u16] }

-- ==
-- entry: sgnu16
-- input { [0u16, 1u16, 65535u16, 8u16, 65528u16] }
-- output { [0u16, 1u16, 1u16, 1u16, 1u16] }

entry negateu16 = map (\x : u16 -> -x)
entry absu16 = map (u16.abs)
entry sgnu16 = map (u16.sgn)
