-- ==
-- input { 0 } output { 1 }
-- input { 1 } output { 2 }

let main (n: i32) =
  length (iota (n+1): []i32)
