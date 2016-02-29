-- This program is a crude implementation of the sequential
-- implementation from Guy Steele's talk "Four Solutions to a Trivial
-- Problem" https://www.youtube.com/watch?v=ftcIcn8AmSY
--
-- It is probably not the nicest way to do this in Futhark, but it
-- found a bug in fusion (related to the 'reverse' function).
--
-- input { [2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1] }
-- output { 35 }

fun int min(int x, int y) =
  if x < y then x else y

fun int max(int x, int y) =
  if x < y then y else x

fun [int,n] reverse([int,n] a) =
  map(fn int (int i) => a[n-i-1], iota(n))

fun int main([int] a) =
  let highestToTheLeft = scan(max, 0, a) in
  let highestToTheRight = reverse(scan(max, 0, reverse(a))) in
  let waterLevels = zipWith(min, highestToTheLeft, highestToTheRight) in
  reduce(+, 0, zipWith(-, waterLevels, a))
