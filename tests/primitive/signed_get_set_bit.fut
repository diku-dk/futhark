-- Test the set_bit and get_bit functions for signed integers.
-- ==
-- entry: test_i8
-- input { 8i8 3 } output { 1 0i8 8i8 }
-- input { 8i8 2 } output { 0 8i8 12i8 }
-- input { 24i8 3 } output { 1 16i8 24i8 }
-- input { 0b010101i8 3 } output { 0 0b010101i8 0b011101i8 }
-- input { 0b11111111i8 7 } output { 1 0b01111111i8 0b11111111i8 }

import "/futlib/math"

entry test_i8 (a:i8) (bit:i32) : (i32, i8, i8) =
  (i8.get_bit bit a,
   i8.set_bit bit a 0,
   i8.set_bit bit a 1)

-- ==
-- entry: test_i16
-- input { 8i16 3 } output { 1 0i16 8i16 }
-- input { 8i16 2 } output { 0 8i16 12i16 }
-- input { 24i16 3 } output { 1 16i16 24i16 }
-- input { 0b0011001001010101i16 11 } output { 0 0b0011001001010101i16 0b0011101001010101i16 }
-- input { 0b1011011010010010i16 13 } output { 1 0b1001011010010010i16 0b1011011010010010i16 }
entry test_i16 (a:i16) (bit:i32) : (i32, i16, i16) =
  (i16.get_bit bit a,
   i16.set_bit bit a 0,
   i16.set_bit bit a 1)

-- ==
-- entry: test_i32
-- input { 8 3 } output { 1 0 8 }
-- input { 8 2 } output { 0 8 12 }
-- input { 24 3 } output { 1 16 24 }
-- input { 214783648 5 } output { 1 214783616 214783648 }
-- input { 214783648 11 } output { 0 214783648 214785696 }
entry test_i32 (a:i32) (bit:i32) : (i32, i32, i32) =
  (i32.get_bit bit a,
   i32.set_bit bit a 0,
   i32.set_bit bit a 1)

-- ==
-- entry: test_i64
-- input { 8i64 3 } output { 1 0i64 8i64 }
-- input { 8i64 2 } output { 0 8i64 12i64 }
-- input { 24i64 3 } output { 1 16i64 24i64 }
-- input { 4294967295i64 31 } output { 1 2147483647i64 4294967295i64 }
-- input { 4294967295i64 30 } output { 1 3221225471i64 4294967295i64 }
entry test_i64 (a:i64) (bit:i32) : (i32, i64, i64) =
  (i64.get_bit bit a,
   i64.set_bit bit a 0,
   i64.set_bit bit a 1)
