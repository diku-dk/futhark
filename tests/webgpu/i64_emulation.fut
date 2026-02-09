-- Test i64 emulation in WebGPU (using vec2<u32>).
-- WebGPU does not support 64-bit integers natively, so they are
-- emulated using pairs of 32-bit values. This tests correctness
-- of arithmetic and overflow behavior.

-- ==
-- entry: add
-- input  { [0i64, 4294967296i64, 9223372036854775807i64, -1i64, 0x00000001FFFFFFFFi64]
--          [0i64, 4294967296i64, 1i64, 1i64, 1i64] }
-- output { [0i64, 8589934592i64, -9223372036854775808i64, 0i64, 0x0000000200000000i64] }

-- ==
-- entry: sub
-- input  { [0i64, 8589934592i64, -9223372036854775808i64, 0i64, 0x0000000200000000i64]
--          [0i64, 4294967296i64, 1i64, 1i64, 1i64] }
-- output { [0i64, 4294967296i64, 9223372036854775807i64, -1i64, 0x00000001FFFFFFFFi64] }

-- ==
-- entry: mul
-- input  { [2i64, 4294967296i64, 0x100000000i64, -1i64, 0x7FFFFFFFi64]
--          [3i64, 2i64, 0x100000000i64, -1i64, 2i64] }
-- output { [6i64, 8589934592i64, 0i64, 1i64, 0xFFFFFFFEi64] }

-- ==
-- entry: div
-- input  { [6i64, 9223372036854775807i64, -9223372036854775808i64, -1i64]
--          [3i64, 2i64, 2i64, 1i64] }
-- output { [2i64, 4611686018427387903i64, -4611686018427387904i64, -1i64] }

-- ==
-- entry: mod
-- input  { [7i64, 9223372036854775807i64, -9223372036854775808i64]
--          [3i64, 1000000000000i64, 1000000000000i64] }
-- output { [1i64, 854775807i64, -854775808i64] }

-- ==
-- entry: neg
-- input { [0i64, 1i64, -1i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i64, -1i64, 1i64, -9223372036854775807i64, -9223372036854775808i64] }

-- ==
-- entry: abs
-- input { [0i64, 1i64, -1i64, 9223372036854775807i64, -9223372036854775808i64] }
-- output { [0i64, 1i64, 1i64, 9223372036854775807i64, -9223372036854775808i64] }

entry add = map2 (i64.+)
entry sub = map2 (i64.-)
entry mul = map2 (i64.*)
entry div = map2 (i64./)
entry mod = map2 (i64.%)
entry neg = map i64.neg
entry abs = map i64.abs
