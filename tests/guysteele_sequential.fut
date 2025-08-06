-- This program is a crude implementation of the sequential
-- implementation from Guy Steele's talk "Four Solutions to a Trivial
-- Problem" https://www.youtube.com/watch?v=ftcIcn8AmSY
--
-- It is probably not the nicest way to do this in Futhark, but it
-- found a bug in fusion (related to the 'reverse' function).
-- ==
-- input { [2,6,3,5,2,8,1,4,2,2,5,3,5,7,4,1] }
-- output { 35 }

def min (x: i32) (y: i32) : i32 =
  if x < y then x else y

def max (x: i32) (y: i32) : i32 =
  if x < y then y else x

def reverse [n] (a: [n]i32) : [n]i32 =
  map (\(i: i64) : i32 -> a[n - i - 1]) (iota (n))

def main (a: []i32) : i32 =
  let highestToTheLeft = scan max 0 a
  let highestToTheRight = reverse (scan max 0 (reverse (a)))
  let waterLevels = map2 min highestToTheLeft highestToTheRight
  in reduce (+) 0 (map2 (-) waterLevels a)
