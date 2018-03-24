-- http://rosettacode.org/wiki/Amicable_pairs
--
-- This program is way too parallel and manifests all the pairs, which
-- requires a giant amount of memory.  Oh well.
--
-- ==
-- tags { no_opencl }
-- compiled input { 300 }
-- output { [[220i32, 284i32]] }

let divisors(n: i32): []i32 =
  filter (\x -> n%x == 0) (1...n/2)

let amicable((n: i32, nd: i32), (m: i32, md: i32)): bool =
  n < m && nd == m && md == n

let getPair [upper] (divs: [upper](i32, i32)) (flat_i: i32): ((i32,i32), (i32,i32)) =
  let i = flat_i / upper
  let j = flat_i % upper
  in unsafe (divs[i], divs[j])

let main(upper: i32): [][2]i32 =
  let range = map (1+) (iota upper)
  let divs = zip range (map (\n -> reduce (+) 0 (divisors n)) range)
  let amicable = filter amicable (map (getPair divs) (iota (upper*upper)))
  in map (\(np,mp) -> [np.1, mp.1]) amicable
