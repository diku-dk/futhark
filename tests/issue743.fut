-- Spurious size annotations maintained by defunctionaliser.
-- ==

let get xs i = xs[i]

let test (xs: []i32) (l: i32): [l]i32 =
    let get_at xs indices = map (get xs) indices
    in get_at xs (iota l)

let main = test (iota 4) 2
