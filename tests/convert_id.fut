-- Test that certain numeric conversions are simplified away.
-- ==
-- structure { ConvOp 4 }

let main (x: i32) (y: u32) =
  (f32 (i64 x),
   i8 (i64 x),
   f32 (u64 x),
   u8 (u64 x))