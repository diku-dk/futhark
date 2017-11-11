-- ==
let f(a: f64): f64 = a + 3.0
let g(a: f64) (b: f64): f64 = a * b

let main(arr: []f64): f64 =
    let n = i32(arr[0])
    let x = map f arr   in
    let arr = loop (arr) for i < n do
        map (g(arr[i])) x
    in arr[0]
