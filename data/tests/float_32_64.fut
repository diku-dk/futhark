-- Test that float32s and float64s can both be used in a program.
--
-- This program does not really test their semantics, but mostly that
-- the parser permits them.
--
-- ==
-- input { 3.14 } output { 3.0 }

fun float32 main(float64 x) =
  toFloat32(trunc64(x))
