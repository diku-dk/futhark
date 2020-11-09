-- A partially applied function whose closure has been consumed.
-- ==
-- error: "QUUX" previously consumed

let const x _ = x

let main(y: i32, QUUX: *[]i32) =
  let f = const QUUX
  let QUUX[1] = 2
  in f y
