// Distribution with maps consuming their input.
//
// --
//
// structure distributed { Map/Loop 0 }

fun [[int,k],n] main(int m, [[int,k],n] a) =
  map(fn [int,k] ([int,k] a_r) =>
        let a_r_copy = copy(a_r) in
        loop(acc = a_r_copy) = for i < m do
          let acc' = copy(zipWith(+, acc, a_r)) in
          let acc'[0] = 0 in
          acc' in
        acc
     , a)
