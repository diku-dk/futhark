-- Test of division-like operators for u32 values.
--
-- ==
-- input {  7u32  3u32 } output {  2u32  1u32  2u32  1u32 }
-- input {  2147483648u32  9u32 } output {  238609294u32  2u32  238609294u32  2u32 }

let main (x: u32) (y: u32): (u32,u32,u32,u32) =
  (x / y, x % y, x // y, x %% y)
