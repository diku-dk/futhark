-- A type becomes existential because a name goes out of scope.
-- ==
-- input { 1i64 } output { 1i64 }

let main n =
  length (let m = n in iota m)
