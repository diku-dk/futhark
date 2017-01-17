-- ==
-- input { 2f32 3f32 }
-- output { 1.674149f32 0.895977f32 }

include futlib.complex

module C = Complex(F32)

fun main(a: f32, b: f32) =
  let x = C.sqrt (C.mk a b)
  in (C.re x, C.im x)
