-- Spurious size annotations maintained by defunctionaliser.
-- ==

let get xs (i: i64) = xs[i]

let test (xs: []i64) (l: i64): [l]i64 =
    let get_at xs indices = map (get xs) indices
    in get_at xs (iota l)

let main = test (iota 4) 2
