// --
// input {
//   [1.0,-4.0,-2.4]
// }
// output {
//   {  -5.4
//   , [2.0, -3.0, -1.4]
//   , [3.0, -7.0, -3.8]
//   }
// }
fun {real,[real],[real]} main([real] arr) =
    let x = map    (+ 1.0, arr) in
    let y = zipWith(+,  x, arr) in
    let r = reduce (+,0.0, arr) in
    {r,x,y}

