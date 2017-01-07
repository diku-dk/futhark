-- https://rosettacode.org/wiki/Loops/For
--
-- Futhark does not have I/O, so this program simply counts in the
-- inner loop.
-- ==
-- input { 10 }
-- output { [0i32, 1i32, 3i32, 6i32, 10i32, 15i32, 21i32, 28i32, 36i32, 45i32] }

fun main(n: int): [n]int =
  loop (a = replicate n 0) = for i < n do
    (loop (s = 0) = for j < i+1 do
     s + j
     let a[i] = s
     in a)
  in a
