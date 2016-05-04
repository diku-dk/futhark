-- Not zip as such, but tests uniqueness of arrays of tuples.

fun (*[[int,m]], *[[int,m]]) main(int n, int m) =
  unzip(map(fn ([i32,m], [i32, m]) (i32 i) =>
              (map(+i, iota(m)), map(-i, iota(m)))
           , iota(n)))
