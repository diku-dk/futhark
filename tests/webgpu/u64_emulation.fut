-- Test u64 emulation in WebGPU (using vec2<u32>).
-- WebGPU does not support 64-bit integers natively, so they are
-- emulated using pairs of 32-bit values. This tests correctness
-- of unsigned arithmetic and overflow/wraparound behavior.

-- ==
-- entry: add
-- input  { [0u64, 4294967296u64, 18446744073709551615u64, 0xFFFFFFFFu64]
--          [0u64, 4294967296u64, 1u64, 1u64] }
-- output { [0u64, 8589934592u64, 0u64, 0x100000000u64] }

-- ==
-- entry: sub
-- input  { [0u64, 8589934592u64, 0u64, 0x100000000u64]
--          [0u64, 4294967296u64, 1u64, 1u64] }
-- output { [0u64, 4294967296u64, 18446744073709551615u64, 0xFFFFFFFFu64] }

-- ==
-- entry: mul
-- input  { [2u64, 4294967296u64, 0x100000000u64, 0xFFFFFFFFFFFFFFFFu64]
--          [3u64, 2u64, 0x100000000u64, 0xFFFFFFFFFFFFFFFFu64] }
-- output { [6u64, 8589934592u64, 0u64, 1u64] }

-- ==
-- entry: div
-- input  { [6u64, 18446744073709551615u64, 18446744073709551615u64]
--          [3u64, 2u64, 18446744073709551615u64] }
-- output { [2u64, 9223372036854775807u64, 1u64] }

-- ==
-- entry: mod
-- input  { [7u64, 18446744073709551615u64, 18446744073709551614u64]
--          [3u64, 1000000000000u64, 18446744073709551615u64] }
-- output { [1u64, 709551615u64, 18446744073709551614u64] }

entry add = map2 (u64.+)
entry sub = map2 (u64.-)
entry mul = map2 (u64.*)
entry div = map2 (u64./)
entry mod = map2 (u64.%)
