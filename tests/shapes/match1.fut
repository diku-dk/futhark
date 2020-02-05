-- Looking at the size of an existential pattern match.
-- ==
-- input {  true 1 2 } output { 1 }
-- input { false 1 2 } output { 2 }

let main b n m =
  let arr = match b
            case true -> iota n
            case false -> iota m
  in length arr
