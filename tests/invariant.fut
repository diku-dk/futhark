-- ==
-- input { 63 }
-- output { 42 }
-- structure { If 0 }

let main(x: i32): i32 =
  assert(x > 32) (if x > 32 then 42 else 1337)
