-- Test the set_bit and get_bit functions for unsigned integers.
-- ==
-- entry: test_u8
-- input { 8u8 3 } output { 1 0u8 8u8 }
-- input { 8u8 2 } output { 0 8u8 12u8 }
-- input { 24u8 3 } output { 1 16u8 24u8 }
-- input { 0b010101u8 3 } output { 0 0b010101u8 0b011101u8 }
-- input { 0b11111111u8 7 } output { 1 0b01111111u8 0b11111111u8 }

import "/futlib/math"

entry test_u8(a:u8, bit:i32) : (i32, u8, u8) =
  (u8.get_bit bit a,
   u8.set_bit bit a 0,
   u8.set_bit bit a 1)

-- ==
-- entry: test_u16
-- input { 8u16 3 } output { 1 0u16 8u16 }
-- input { 8u16 2 } output { 0 8u16 12u16 }
-- input { 24u16 3 } output { 1 16u16 24u16 }
-- input { 0b0011001001010101u16 11 } output { 0 0b0011001001010101u16 0b0011101001010101u16 }
-- input { 0b1011011010010010u16 13 } output { 1 0b1001011010010010u16 0b1011011010010010u16 }
entry test_u16(a:u16, bit:i32) : (i32, u16, u16) =
  (u16.get_bit bit a,
   u16.set_bit bit a 0,
   u16.set_bit bit a 1)

-- ==
-- entry: test_u32
-- input { 8u32 3 } output { 1 0u32 8u32 }
-- input { 8u32 2 } output { 0 8u32 12u32 }
-- input { 24u32 3 } output { 1 16u32 24u32 }
-- input { 0b0011001001010101u32 11 } output { 0 0b0011001001010101u32 0b0011101001010101u32 }
-- input { 0b11111111u32 7 } output { 1 0b01111111u32 0b11111111u32 }
entry test_u32(a:u32, bit:i32) : (i32, u32, u32) =
  (u32.get_bit bit a,
   u32.set_bit bit a 0,
   u32.set_bit bit a 1)

-- ==
-- entry: test_u64
-- input { 8u64 3 } output { 1 0u64 8u64 }
-- input { 8u64 2 } output { 0 8u64 12u64 }
-- input { 24u64 3 } output { 1 16u64 24u64 }
-- input { 0b0011001001010101u64 11 } output { 0 0b0011001001010101u64 0b0011101001010101u64 }
-- input { 0b11111111u64 7 } output { 1 0b01111111u64 0b11111111u64 }
-- input { 4294967295u64 31 } output { 1 2147483647u64 4294967295u64 }
-- input { 4294967295u64 30 } output { 1 3221225471u64 4294967295u64 }
entry test_u64(a:u64, bit:i32) : (i32, u64, u64) =
  (u64.get_bit bit a,
   u64.set_bit bit a 0,
   u64.set_bit bit a 1)
