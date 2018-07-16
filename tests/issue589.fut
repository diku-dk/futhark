-- The problem is that we currently do not alias xs_i to xs, but they
-- really do alias once we replace the polymorphic type t with []i32.
-- Required extra conservatism in the type checker.
-- ==
-- error: in-place modification

let swap 't (i: i32) (j: i32) (xs: *[]t) =
  let xs_i = xs[i]
  let xs[i] = xs[j]
  let xs[j] = xs_i
  in xs

let main (xs: *[][]i32) = swap 0 1 xs
