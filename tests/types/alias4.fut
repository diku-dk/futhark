-- An array type alias can be unique.

type matrix = [][]int

fun main(m: *matrix): matrix =
  let m[0,0] = 0 in m
