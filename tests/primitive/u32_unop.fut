-- Test unary operators for u32.

-- ==
-- entry: negateu32
-- input { [0u32, 1u32, 4294967295u32, 8u32, 4294967288u32] }
-- output { [0u32, 4294967295u32, 1u32, 4294967288u32, 8u32] }

-- ==
-- entry: absu32
-- input { [0u32, 1u32, 4294967295u32, 8u32, 4294967288u32] }
-- output { [0u32, 1u32, 4294967295u32, 8u32, 4294967288u32] }

-- ==
-- entry: sgnu32
-- input { [0u32, 1u32, 4294967295u32, 8u32, 4294967288u32] }
-- output { [0u32, 1u32, 1u32, 1u32, 1u32] }

entry negateu32 = map (\x : u32 -> -x)
entry absu32 = map (u32.abs)
entry sgnu32 = map (u32.sgn)
