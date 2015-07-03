// Simplified variant of loop2.fut with lower-rank arrays.
//
// --
//
// structure distributed { Map/Loop 0 }

fun [[int,k],n] main(int m, [[int,k],n] a) =
  map(fn [int,k] ([int,k] a_r) =>
        let acc = a_r in
        loop(acc) = for i < m do
          zipWith(+, acc, a_r) in
        acc
     , a)
