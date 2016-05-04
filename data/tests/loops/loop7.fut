-- This loop is intended to trigger a bug in the in-place-lowering
-- optimisation.  It requires proper maintaining of the loop result
-- ordering.
-- ==

fun [[f64]] main(int n, int i, f64 x) =
    let res = replicate(n,(replicate(n,0.0))) in
    let (u, uu) = (replicate(n,0.0),
                   replicate(n,0.0)) in
    loop ((u, x)) =
      for i < n-1 do
        let y = x + 1.0 in
        let u[i] = u[i] * y in
        (u, y) in
    let res[i] = u in
    res
