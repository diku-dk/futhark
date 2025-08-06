-- http://rosettacode.org/wiki/Amicable_pairs
--
-- This program is way too parallel and manifests all the pairs, which
-- requires a giant amount of memory.  Oh well.
--
-- ==
-- tags { no_ispc }
-- compiled input { 300i64 }
-- output { [[220i32, 284i32]] }

def divisors (n: i32) : []i32 =
  filter (\x -> n % x == 0) (1...n / 2 + 1)

def amicable ((n: i32, nd: i32), (m: i32, md: i32)) : bool =
  n < m && nd == m && md == n

def getPair [upper] (divs: [upper](i32, i32)) (flat_i: i64) : ((i32, i32), (i32, i32)) =
  let i = flat_i / upper
  let j = flat_i % upper
  in (divs[i], divs[j])

def main (upper: i64) : [][2]i32 =
  let range = map (1 +) (iota upper)
  let divs =
    zip (map i32.i64 range)
        (map (\n -> reduce (+) 0 (divisors (i32.i64 n))) range)
  let amicable = filter amicable (map (getPair divs) (iota (upper * upper)))
  in map (\((x, _), (y, _)) -> [x, y]) amicable
