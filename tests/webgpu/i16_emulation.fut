-- Test i16 emulation in WebGPU (packed into i32).
-- WebGPU does not support i8/i16 natively, so they are emulated
-- by packing into i32. This tests boundary conditions and correctness.

-- ==
-- entry: add
-- input  { [0i16, 32767i16, -32768i16, 32767i16, -1i16]
--          [0i16, 1i16, -1i16, -32768i16, -1i16] }
-- output { [0i16, -32768i16, 32767i16, -1i16, -2i16] }

-- ==
-- entry: sub
-- input  { [0i16, -32768i16, 32767i16, 0i16]
--          [1i16, 1i16, -1i16, -32768i16] }
-- output { [-1i16, 32767i16, -32768i16, -32768i16] }

-- ==
-- entry: mul
-- input  { [2i16, -2i16, 256i16, -256i16, 181i16]
--          [3i16, 3i16, 128i16, 128i16, 181i16] }
-- output { [6i16, -6i16, -32768i16, -32768i16, 32761i16] }

-- ==
-- entry: div
-- input  { [6i16, -6i16, 32767i16, -32768i16, -32768i16]
--          [3i16, 3i16, 2i16, 2i16, -1i16] }
-- output { [2i16, -2i16, 16383i16, -16384i16, -32768i16] }

-- ==
-- entry: mod
-- input  { [7i16, -7i16, 32767i16, -32768i16]
--          [3i16, 3i16, 100i16, 100i16] }
-- output { [1i16, -1i16, 67i16, -68i16] }

-- ==
-- entry: neg
-- input { [0i16, 1i16, -1i16, 32767i16, -32768i16] }
-- output { [0i16, -1i16, 1i16, -32767i16, -32768i16] }

-- ==
-- entry: abs
-- input { [0i16, 1i16, -1i16, 32767i16, -32768i16] }
-- output { [0i16, 1i16, 1i16, 32767i16, -32768i16] }

entry add = map2 (i16.+)
entry sub = map2 (i16.-)
entry mul = map2 (i16.*)
entry div = map2 (i16./)
entry mod = map2 (i16.%)
entry neg = map i16.neg
entry abs = map i16.abs
