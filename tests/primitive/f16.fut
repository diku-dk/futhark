-- Test ad-hoc properties and utility functions for f16.
--
-- ==
-- input {  1f16 0f16 } output { true false 0x3c00u16 1f16 }
-- input { -1f16 0f16 } output { true true 0xbc00u16 -1f16 }
-- input { -1f16 1f16 } output { false true 0xbc00u16 -1f16 }


def main (x: f16) (y: f16): (bool, bool, u16, f16) =
  (f16.isinf(x / y),
   f16.isnan(f16.sqrt(x)),
   f16.to_bits x,
   f16.from_bits (f16.to_bits x))
