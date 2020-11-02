-- ==
-- input { [1,2,3] } output { [0,1,2] [1,2,3] }

type size [n] = [n]()

let size n = replicate n ()

let iota' [n] (_: size [n]) : [n]i32 =
  0..1..<i32.i64 n :> [n]i32

let length' [n] 'a (_: [n]a) : size [n] =
  size n

let f xs = zip (iota' (length' xs)) xs

let main (xs: []i32) = unzip (f xs)
