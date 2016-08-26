-- Test of division-like operators for u8 values.
--
-- ==
-- input {  7u8  3u8 } output {  2u8  1u8  2u8  1u8 }
-- input {  128u8  9u8 } output {  14u8  2u8  14u8  2u8 }

fun main(x: u8, y: u8): (u8,u8,u8,u8) =
  (x / y, x % y, x // y, x %% y)
