-- More complicated scanomap example.  Distilled from radix sort.
-- ==
--
-- input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [4339, 4586, 4929, 5654, 6120, 6554, 7046, 7535]
-- }

def step [n] (xs: [n]i32) : [n]i32 =
  let bits = map (+ 1) xs
  let ps1 = scan (+) 0 bits
  let bits_sum = reduce (+) 0 bits
  let ps1' = map (+ bits_sum) ps1
  let xs' = map2 (+) (ps1') xs
  in xs'

def main [n] (xs: [n]i32) : [n]i32 =
  loop (xs) for i < 2 do
    step (xs)
