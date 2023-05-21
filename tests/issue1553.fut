let argmax arr =
  reduce_comm (\(a,i) (b,j) ->
                 if a < b
                 then (b,j)
                 else if b < a then (a,i)
                 else if j < i then (b,j)
                 else (a,i))
              (0f32, 0)
              (zip arr (indices arr))

let gaussian_elimination [n] [m] (A: [m][n]f32): [m][n]f32 =
  loop A for i < i64.min m n do
    let value j x = if j >= i then f32.abs x else -f32.inf
    let j = A[:,i] |> map2 value (indices A) |> argmax |> (.1)
    let f = (1-A[i,i]) / A[j,i]
    let irow = map2 (f32.fma f) A[j] A[i]
    in tabulate m (\j ->
                     let f = A[j,i] * -1
                     in map2 (\x y -> if j == i then x else f32.fma f x y)
                             irow A[j])
let createIdentity n =
  tabulate n (\i -> replicate n 0f32 with [i] = 1f32)

let matrix_inverse [n] (A: [n][n]f32): [n][n]f32 =
  let I  = createIdentity n
  let AI = map2 concat A I
  let A1I= gaussian_elimination AI
  let A1 = A1I[:,n:] :> [n][n]f32
  in A1

let main [k] [n] (As: [k][n][n]f32) :[k][n][n]f32 =
  map (\A -> matrix_inverse A) As
