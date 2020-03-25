-- Overloaded numbers should not track aliases.

let main =
  let arr = [3,7]
  let a = arr[0]
  let arr[0] = 0
  in a
