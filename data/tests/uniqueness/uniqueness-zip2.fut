-- Not zip as such, but tests uniqueness of arrays of tuples.

fun (*[][m]int, *[][m]int) main(int n, int m) =
  unzip(map(fn ([m]i32, [m]i32) (i32 i) =>
              (map(+i, iota(m)), map(-i, iota(m)))
           , iota(n)))
