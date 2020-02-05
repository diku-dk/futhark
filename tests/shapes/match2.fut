-- Size hidden by match.
-- ==
-- input { 2 } output { 2 }

let main (n: i32) =
  let arr = match n
            case m -> iota m
  in length arr
