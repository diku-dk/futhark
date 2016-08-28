-- Not zip as such, but tests uniqueness of arrays of tuples.

fun main(n: int, m: int): (*[][m]int, *[][m]int) =
  unzip(map (fn (i: i32): ([m]i32, [m]i32)  =>
              (map (+i) (iota(m)), map (-i) (iota(m)))
           ) (iota(n)))
