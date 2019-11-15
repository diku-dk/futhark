-- ==
-- input { [[0], [1]] }
-- output { [[1], [0]] }

let swap [n] 't (i: i32) (j: i32) (xs: *[n]t) =
  let xs_i = copy xs[i]
  let xs_j = copy xs[j]
  let xs[i] = xs_j
  let xs[j] = xs_i
  in xs

let main (xs: *[][]i32) = swap 0 1 xs
