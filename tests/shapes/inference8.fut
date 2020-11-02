-- Just because a top-level binding tries to hide its size (which is
-- existential), that does not mean it gets to have a blank size.
-- ==
-- input { 2i64 } output { [0i64,1i64] }

let arr : []i64 = iota (10+2)

let main (n: i64) =
  copy (take n arr)
