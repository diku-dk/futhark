-- Test f16 native support in WebGPU.
-- WebGPU supports f16 natively (with shader-f16 extension),
-- unlike some other GPU backends. This tests f16 operations.

-- ==
-- entry: add
-- input  { [0f16, 1f16, -1f16, 65504f16, -65504f16]
--          [0f16, 1f16, 1f16, 1f16, -1f16] }
-- output { [0f16, 2f16, 0f16, 65504f16, -65504f16] }

-- ==
-- entry: sub
-- input  { [0f16, 2f16, 0f16, 65504f16]
--          [0f16, 1f16, 1f16, 1f16] }
-- output { [0f16, 1f16, -1f16, 65504f16] }

-- ==
-- entry: mul
-- input  { [0f16, 2f16, -2f16, 256f16, 0.001f16]
--          [1f16, 3f16, 3f16, 256f16, 0.001f16] }
-- output { [0f16, 6f16, -6f16, 65504f16, 0f16] }

-- ==
-- entry: div
-- input  { [0f16, 6f16, -6f16, 1f16]
--          [1f16, 3f16, 3f16, 65504f16] }
-- output { [0f16, 2f16, -2f16, 0f16] }

-- ==
-- entry: neg
-- input { [0f16, 1f16, -1f16, 65504f16, f16.inf, f16.nan] }
-- output { [0f16, -1f16, 1f16, -65504f16, -f16.inf, f16.nan] }

-- ==
-- entry: abs
-- input { [0f16, 1f16, -1f16, 65504f16, -65504f16, f16.inf, -f16.inf] }
-- output { [0f16, 1f16, 1f16, 65504f16, 65504f16, f16.inf, f16.inf] }

-- ==
-- entry: sqrt
-- input { [0f16, 1f16, 4f16, 9f16, 16f16, 65504f16] }
-- output { [0f16, 1f16, 2f16, 3f16, 4f16, 256f16] }

-- ==
-- entry: fma
-- input  { [2f16, 3f16, 100f16]
--          [3f16, 4f16, 100f16]
--          [1f16, 1f16, 1f16] }
-- output { [7f16, 13f16, 10001f16] }

entry add = map2 (f16.+)
entry sub = map2 (f16.-)
entry mul = map2 (f16.*)
entry div = map2 (f16./)
entry neg = map f16.neg
entry abs = map f16.abs
entry sqrt = map f16.sqrt
entry fma (a: []f16) (b: []f16) (c: []f16) = map3 f16.fma a b c
