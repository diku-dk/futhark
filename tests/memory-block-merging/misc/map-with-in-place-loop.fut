-- A tricky case for array coalescing.


import "/futlib/array"

let mapper (_arg: i32): []i32 =
  let xs = replicate 1 0

  loop (xs) =
    for _i < 2 do
      let xs[0] = xs[0]
      in xs

  loop (xs) =
    for _i < 2 do
      let xs[0] = xs[0]
      in xs

  in xs

let main (ys: []i32): [][]i32 =
  map mapper ys
