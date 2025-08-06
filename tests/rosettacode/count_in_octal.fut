-- https://rosettacode.org/wiki/Count_in_octal
--
-- Futhark cannot print.  Instead we produce an array of integers that
-- look like octal numbers when printed in decimal.
--
-- ==
-- input { 20i64 }
-- output { [0i32, 1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 10i32, 11i32,
--           12i32, 13i32, 14i32, 15i32, 16i32, 17i32, 20i32, 21i32, 22i32, 23i32] }

def octal (x: i64) : i32 =
  let (out, _, _) =
    loop (out, mult, x) = (0, 1, i32.i64 x)
    while x > 0 do
      let digit = x % 8
      let out = out + digit * mult
      in (out, mult * 10, x / 8)
  in out

def main (n: i64) : [n]i32 =
  map octal (iota n)
