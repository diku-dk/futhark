-- Simplified variant of loop2.fut with lower-rank arrays.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

fun [n][k]int main(int m, [n][k]int a) =
  map(fn [k]int ([k]int a_r) =>
        let acc = a_r in
        loop(acc) = for i < m do
          zipWith(+, acc, a_r) in
        acc
     , a)
