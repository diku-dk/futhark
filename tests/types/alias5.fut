-- Uniqueness goes outside-in.

type uniqlist [n] = *[n]i32

let main(p: [][]i32): [](uniqlist []) =
  p
