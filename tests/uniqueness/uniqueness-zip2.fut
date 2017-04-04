-- Not zip as such, but tests uniqueness of arrays of tuples.

let main(n: i32, m: i32): (*[][m]i32, *[][m]i32) =
  unzip(map (\(i: i32): ([m]i32, [m]i32)  ->
              (map (+i) (iota(m)), map (-i) (iota(m)))
           ) (iota(n)))
