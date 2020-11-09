-- Could fail after internalisation.

let consume (arr: *[](i32, i32)) = arr

let main (arr: *[](i32, i32)) =
  let arr' = rotate 1 arr
  in consume arr
