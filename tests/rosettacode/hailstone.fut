-- https://rosettacode.org/wiki/Hailstone_sequence
--
-- Does not use memoization, but is instead parallel for task #3.
--
-- ==
-- compiled input { 27 100000 }
-- output {
--   [27i32, 82i32, 41i32, 124i32, 62i32, 31i32, 94i32, 47i32, 142i32,
--    71i32, 214i32, 107i32, 322i32, 161i32, 484i32, 242i32, 121i32,
--    364i32, 182i32, 91i32, 274i32, 137i32, 412i32, 206i32, 103i32,
--    310i32, 155i32, 466i32, 233i32, 700i32, 350i32, 175i32, 526i32,
--    263i32, 790i32, 395i32, 1186i32, 593i32, 1780i32, 890i32,
--    445i32, 1336i32, 668i32, 334i32, 167i32, 502i32, 251i32, 754i32,
--    377i32, 1132i32, 566i32, 283i32, 850i32, 425i32, 1276i32,
--    638i32, 319i32, 958i32, 479i32, 1438i32, 719i32, 2158i32,
--    1079i32, 3238i32, 1619i32, 4858i32, 2429i32, 7288i32, 3644i32,
--    1822i32, 911i32, 2734i32, 1367i32, 4102i32, 2051i32, 6154i32,
--    3077i32, 9232i32, 4616i32, 2308i32, 1154i32, 577i32, 1732i32,
--    866i32, 433i32, 1300i32, 650i32, 325i32, 976i32, 488i32, 244i32,
--    122i32, 61i32, 184i32, 92i32, 46i32, 23i32, 70i32, 35i32,
--    106i32, 53i32, 160i32, 80i32, 40i32, 20i32, 10i32, 5i32, 16i32,
--    8i32, 4i32, 2i32, 1i32]
--
--    351i32
-- }

def hailstone_step (x: i32) : i32 =
  if (x % 2) == 0
  then x / 2
  else (3 * x) + 1

def hailstone_seq (x: i32) : []i32 =
  let capacity = 100
  let i = 1
  let steps = replicate capacity (-1)
  let steps[0] = x
  let (_, i, steps, _) =
    loop ((capacity, i, steps, x)) while x != 1 do
      let (steps, capacity) =
        if i == capacity
        then ( concat steps (replicate capacity (-1))
             , capacity * 2
             )
        else (steps, capacity)
      let x = hailstone_step x
      let steps[i] = x
      in (capacity, i + 1, steps, x)
  in take i steps

def hailstone_len (x: i32) : i32 =
  (loop (i, x) = (1, x) while x != 1 do (i + 1, hailstone_step x)).0

def max (x: i32) (y: i32) : i32 = if x < y then y else x

def main (x: i32) (n: i32) : ([]i32, i32) =
  ( hailstone_seq x
  , reduce max 0 (map hailstone_len
                      (map (1 +) (map i32.i64 (iota (i64.i32 n - 1)))))
  )
