-- Do not let the uniqueness of a returned array affect the inferred
-- uniqueness of a function parameter.

let (>->) '^a '^b '^c (f: a -> b) (g: b -> c) (x: a): c = g (f x)

let tabmap [n] 'a 'b (f: i32 -> a -> b) (xs: [n]a): *[1]b =
  [f 0 xs[0]]

let main [n][m] (arr: [n][m]f32): [][]f32 =
  let f (i: i32) (j: i32) (x: f32) = x
  in tabmap (f >-> tabmap) arr
