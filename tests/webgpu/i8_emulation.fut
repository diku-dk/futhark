-- Test i8 emulation in WebGPU (packed into i32).
-- WebGPU does not support i8/i16 natively, so they are emulated
-- by packing into i32. This tests boundary conditions and correctness.

-- ==
-- entry: add
-- input  { [0i8, 127i8, -128i8, 127i8, -1i8]
--          [0i8, 1i8, -1i8, -128i8, -1i8] }
-- output { [0i8, -128i8, 127i8, -1i8, -2i8] }

-- ==
-- entry: sub
-- input  { [0i8, -128i8, 127i8, 0i8]
--          [1i8, 1i8, -1i8, -128i8] }
-- output { [-1i8, 127i8, -128i8, -128i8] }

-- ==
-- entry: mul
-- input  { [2i8, -2i8, 127i8, -128i8, 16i8]
--          [3i8, 3i8, 2i8, 2i8, 8i8] }
-- output { [6i8, -6i8, -2i8, 0i8, -128i8] }

-- ==
-- entry: div
-- input  { [6i8, -6i8, 127i8, -128i8, -128i8]
--          [3i8, 3i8, 2i8, 2i8, -1i8] }
-- output { [2i8, -2i8, 63i8, -64i8, -128i8] }

-- ==
-- entry: mod
-- input  { [7i8, -7i8, 127i8, -128i8]
--          [3i8, 3i8, 10i8, 10i8] }
-- output { [1i8, 2i8, 7i8, 2i8] }

-- ==
-- entry: neg
-- input { [0i8, 1i8, -1i8, 127i8, -128i8] }
-- output { [0i8, -1i8, 1i8, -127i8, -128i8] }

-- ==
-- entry: abs
-- input { [0i8, 1i8, -1i8, 127i8, -128i8] }
-- output { [0i8, 1i8, 1i8, 127i8, -128i8] }

entry add = map2 (i8.+)
entry sub = map2 (i8.-)
entry mul = map2 (i8.*)
entry div = map2 (i8./)
entry mod = map2 (i8.%)
entry neg = map i8.neg
entry abs = map i8.abs
