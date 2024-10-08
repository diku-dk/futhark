-- Bitwise operation stress test.
--
-- Mostly to ensure that interpreter and code generator agree.
-- Originally distilled from MD5 sum calculation.
-- ==
-- input {
--   1732584193u32
--   -271733879u32
--   -1732584194u32
--   271733878u32
--   1
-- }
-- output {
--   271733878u32
--   757607282u32
--   -271733879u32
--   -1732584194u32
-- }
def funF (x: u32, y: u32, z: u32): u32 = x & y | notx & z

def rotateL (x: u32, i: u32): u32 =
  let post = x << i
  let pre = (x >> i) & (not(4294967295 << i))
  in post | pre

def frob (a: u32, b: u32, c: u32, d: u32): (u32, u32, u32, u32) =
  let w = 2543360272
  let f' = funF (b, c, d)
  let a' = b + rotateL ((a + f' + w + 3614090360), 7)
  in (d, a', b, c)

def main (a: u32) (b: u32) (c: u32) (d: u32) (n: i32): (u32, u32, u32, u32) =
  loop (a', b', c', d') = (a, b, c, d) for _i < n do
    frob (a', b', c', d')