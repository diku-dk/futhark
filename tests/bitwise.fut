-- Bitwise operation stress test.
--
-- Mostly to ensure that interpreter and code generator agree.
-- Originally distilled from MD5 sum calculation.
-- ==
-- input {
--   1732584193
--   -271733879
--   -1732584194
--   271733878
--   1
-- }
-- output {
--   271733878
--   757607282
--   -271733879
--   -1732584194
-- }

fun funF(x: i32, y: i32, z: i32): i32 = x & y | ~x & z

fun rotateL (x: i32, i: i32): i32 =
  let post = x << i
  let pre = (x >> i) & (~(0xFFFFFFFF << i)) in
  post | pre

fun frob(a: i32, b: i32, c: i32, d: i32): (i32, i32, i32, i32) =
  let w = 0x97989910
  let f' = funF(b,c,d)
  let a' = b + rotateL((a + f' + w + 0xd76aa478), 7) in
  (d, a', b, c)

fun main(a: i32, b: i32, c: i32, d: i32, n: i32): (i32, i32, i32, i32) =
  loop ((a',b',c',d') = (a,b,c,d)) = for i < n do
    frob(a',b',c',d')
  in (a', b', c', d')
