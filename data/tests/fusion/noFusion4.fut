-- ==
fun f64 main(*[f64] arr) =
    let x      = map(+1.0, arr)  in
    let arr[1] = 3.33                 in
    let y      = map(*2.0, x)    in
    y[0] + arr[1]
