-- Test that we can access intrinsics through the intrinsics module.
-- ==
-- input { 5 } output { 7.0f32 10 }

fun main(x: i32): (intrinsics.f32, intrinsics.i32) =
  (intrinsics.f32 x intrinsics.+ 2.0f32,
   x intrinsics.+ x)