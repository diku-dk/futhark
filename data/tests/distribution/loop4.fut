-- Distribution with maps consuming their input.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

fun [n][k]int main(int m, [n][k]int a) =
  map(fn [k]int ([k]int a_r) =>
        let a_r_copy = copy(a_r) in
        loop(acc = a_r_copy) = for i < m do
          let acc' = copy(zipWith(+, acc, a_r)) in
          let acc'[0] = 0 in
          acc' in
        acc
     , a)
