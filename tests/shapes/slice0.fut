-- Multiple slices with the same operands produce things that have the
-- same size.

let f (x: i32) = x + 2
let g (x: i32) = x * 2

let main [n] (xs: [n]i32) (ys: [n]i32) (i: i32) (j: i32) =
  zip xs[(f i):(g j)] ys[(f i):(g j)]
