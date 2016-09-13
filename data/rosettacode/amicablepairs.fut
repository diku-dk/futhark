-- http://rosettacode.org/wiki/Amicable_pairs
--
-- This program is way too parallel and manifests all the pairs, which
-- requires a giant amount of memory.  Oh well.
--
-- ==
-- compiled input { 300 }
-- output { [[220i32, 284i32]] }

fun divisors(n: int): []int =
  filter (fn x => n%x == 0) (map (1+) (iota (n/2)))

fun amicable((n: int, nd: int), (m: int, md: int)): bool =
  n < m && nd == m && md == n

fun getPair (divs: [upper](int, int)) (flat_i: int): ((int,int), (int,int)) =
  let i = flat_i / upper
  let j = flat_i % upper
  in unsafe (divs[i], divs[j])

fun main(upper: int): [][2]int =
  let range = map (1+) (iota upper)
  let divs = zip range (map (fn n => reduce (+) 0 (divisors n)) range)
  let amicable = filter amicable (map (getPair divs) (iota (upper*upper)))
  in map (fn (np,mp) => [np.0, mp.0]) amicable
