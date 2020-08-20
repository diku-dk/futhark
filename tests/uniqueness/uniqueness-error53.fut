-- Do not hide global variables with flatten.
-- ==
-- error: Cannot apply

let xss : [][]i32 = [[1,2,3],[4,5,6]]

let main (n: i32): *[]i32 =
  let xs = flatten xss
  in scatter xs (iota n) (iota n)
