-- A type becomes existential because a name goes out of scope,
-- trickier.
-- ==
-- input { 1 } output { 2 }

let main n =
  length (let m = n+1 in iota m)
