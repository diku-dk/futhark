-- Memory block merging with a concat of two arrays of different sources into a
-- multidimensional array.
-- ==
-- input {  [ [1i32, 1i32, 1i32, 1i32]
--          , [1i32, 1i32, 1i32, 1i32]
--          ]
--          [3, 7]
--          [8, 9]
--       }
-- output {
--          [ [4i32, 8i32, 9i32, 10i32]
--          , [1i32, 1i32, 1i32,  1i32]
--          ]
--        }

let main (y: *[#n][#q]i32, a: []i32, b: []i32): *[n][q]i32 =
  let a1 = map (+1) a
  let b1 = map (+1) b
  let z = concat a1 b1
  let y[0] = z
  in  y
