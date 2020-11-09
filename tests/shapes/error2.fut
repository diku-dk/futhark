-- An implied/unknown size can still only be inferred to have one concrete size.
-- ==
-- error: (\[1\]i32, \[2\]i32)

let main (xs: []i32) : ([1]i32, [2]i32) = (xs, xs)
