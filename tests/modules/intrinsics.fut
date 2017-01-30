-- Test that we can access intrinsics through the Intrinsics module.
-- ==
-- input { 5 } output { 7.0f32 10 }

fun main(x: i32): (Intrinsics.f32, Intrinsics.i32) =
  (Intrinsics.f32 x Intrinsics.+ 2.0f32,
   x Intrinsics.+ x)