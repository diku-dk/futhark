-- Just because a top-level binding tries to hide its size, that does
-- not mean it gets to have a blank size.
-- ==
-- input { 2 } output { [0,1] }

let arr : []i32 = iota 10

let main (n: i32) =
  copy (take n arr)
