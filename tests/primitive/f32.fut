-- Test ad-hoc properties and utility functions for f32.
--
-- ==
-- input {  1f32 0f32 } output { true false 0x3f800000u32 1f32 }
-- input { -1f32 0f32 } output { true true 0xbf800000u32 -1f32 }
-- input { -1f32 1f32 } output { false true 0xbf800000u32 -1f32 }


let main (x: f32) (y: f32): (bool, bool, u32, f32) =
  (f32.isinf(x / y),
   f32.isnan(f32.sqrt(x)),
   f32.to_bits x,
   f32.from_bits (f32.to_bits x))
