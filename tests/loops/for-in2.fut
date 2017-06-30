-- For-in loop where replicate should be optimised away.
-- ==
-- input { 5 }
-- output { 99 }
-- structure { Replicate 0 }

let main(n: i32) =
  let xs = replicate n n in
  loop (a=0) for x in xs do (a<<1) ^ x
