-- Based on issue #1460

let update [n] [m] (mat: *[n][m]i64): *[n][m]i64 =
    let mat = rotate (-1) mat
    in mat with [n-1] = iota m

let run (t:i64) (n:i64) (m:i64): [n][m]i64 =
    let mat = tabulate_2d n m (+) in
    loop (mat) for i < t do update mat

let main (t:i64) (n:i64) (m:i64) =
    map (\i -> run t n m) (iota 10)
