-- For-in loop where iota should be optimised away.
-- ==
-- input { 5 }
-- output { 4 }
-- structure { Iota 0 }

let main(n: i32) =
  let xs = iota n in
  loop (a=0) for x in xs do a ^ x
