-- https://rosettacode.org/wiki/Binary_digits
--
-- We produce the binary number as a 64-bit integer whose digits are
-- all 0s and 1s - this is because Futhark does not have any way to
-- print, nor strings for that matter.
--
-- ==
-- input { 5 }
-- output { 101i64 }
-- input { 50 }
-- output { 110010i64 }
-- input { 9000 }
-- output { 10001100101000i64 }

def main (x: i32) : i64 =
  loop out = 0i64
  for i < 32 do
    let digit = (x >> (31 - i)) & 1
    let out = (out * 10i64) + i64.i32 (digit)
    in out
