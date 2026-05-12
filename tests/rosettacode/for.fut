-- https://rosettacode.org/wiki/Loops/For
--
-- Futhark does not have I/O, so this program simply counts in the
-- inner loop.
-- ==
-- input { 10i64 }
-- output { [0i64, 1i64, 3i64, 6i64, 10i64, 15i64, 21i64, 28i64, 36i64, 45i64] }

def main (n: i64) : [n]i64 =
  loop a = replicate n 0
  for i < n do
    (let a[i] = loop s = 0 for j < i + 1 do s + j
     in a)
