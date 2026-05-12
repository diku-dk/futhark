-- Test that certain numeric conversions are simplified away.
-- ==
-- structure { ConvOp 4 }

def main (x: i32) (y: u32) =
  ( f32.i64 (i64.i32 x)
  , i8.i64 (i64.i32 x)
  , f32.u64 (u64.i32 x)
  , u8.u64 (u64.i32 x)
  )
