-- More tricky variant of loop0.fut where expanding the initial merge
-- parameter values is not so simple.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

fun [[int,k],n] main([[[int,k],m],n] a) =
  map(fn [int,k] ([[int,k],m] a_r) =>
        let acc = a_r[0] in
        loop(acc) = for i < m do
          zipWith(+, acc, a_r[i]) in
        acc
     , a)
