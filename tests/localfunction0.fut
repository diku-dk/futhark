-- A simple locally defined function.
-- ==
-- input { [1,2,3] } output { [3,4,5] }

let main(a: [n]i32) =
  let add_two (x: i32) = x + 2
  in map add_two a
