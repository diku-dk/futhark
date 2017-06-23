-- In-place lowering.

let main (n: i32, k: i32): [n][n]i32 =
  let r0 = replicate n 1
  let x = replicate n (replicate n 2)

  loop (r = r0) = for i < n do
    let a = r[i] in
    let r[i] = a * i in
    r
    in
  let x[k] = r in
  x
