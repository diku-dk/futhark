-- For-in loop where map and iota should be optimised away.
-- ==
-- input { 5 }
-- output { 2 }
-- structure { Iota 0 Map 0 }

let main(n: i32) =
  let xs = map (2*) (map (1+) (iota n)) in
  loop (a=0) for x in xs do a ^ x
