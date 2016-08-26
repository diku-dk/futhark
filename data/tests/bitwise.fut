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

fun funF(x: int, y: int, z: int): int = x & y | ~x & z

fun rotateL (x: int, i: int): int =
  let post = x << i in
  let pre = (x >> i) & (~(0xFFFFFFFF << i)) in
  post | pre

fun frob(a: int, b: int, c: int, d: int): (int, int, int, int) =
  let w = 0x97989910 in
  let f' = funF(b,c,d) in
  let a' = b + rotateL((a + f' + w + 0xd76aa478), 7) in
  (d, a', b, c)

fun main(a: int, b: int, c: int, d: int, n: int): (int, int, int, int) =
  loop ((a',b',c',d') = (a,b,c,d)) = for i < n do
    frob(a',b',c',d')
  in (a', b', c', d')
