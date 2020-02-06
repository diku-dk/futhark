-- A type becomes existential because a name goes out of scope.
-- ==
-- input { 1 } output { 1 }

let main n =
  length (let m = n in iota m)
