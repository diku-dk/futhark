// This loop is intended to trigger a bug in the in-place-lowering
// optimisation.  It requires proper maintaining of the loop result
// ordering.

fun [[real]] main(int n, int i, real x) =
    let res = copy(replicate(n,(replicate(n,0.0)))) in
    let {u, uu} = {copy(replicate(n,0.0)),
                   copy(replicate(n,0.0))} in
    loop ({u, x}) =
      for i < n-1 do
        let y = x + 1.0 in
        let u[i] = u[i] * y in
        {u, y} in
    let res[i] = u in
    res
