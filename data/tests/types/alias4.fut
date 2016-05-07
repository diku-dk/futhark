-- An array type alias can be unique.

type matrix = [[int]]

fun matrix main(*matrix m) =
  let m[0,0] = 0 in m
