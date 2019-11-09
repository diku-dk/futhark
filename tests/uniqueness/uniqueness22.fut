-- Once failed after internalisation.

let main (arr: *[](i32, i32)) =
  let arr' = rotate 1 arr
  in arr' with [0] = (0,0)
