-- Looking at the size of an existential branch.
-- ==
-- input {  true 1 2 } output { 1 }
-- input { false 1 2 } output { 2 }

let main b n m =
  length (if b then iota n else iota m)
