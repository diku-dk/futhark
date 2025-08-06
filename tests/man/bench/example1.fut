-- How quickly can we reduce arrays?
--
-- ==
-- nobench input { 0i64 }
-- output { 0i64 }
-- input { 100i64 }
-- output { 4950i64 }
-- compiled input { 10000i64 }
-- output { 49995000i64 }
-- compiled input { 1000000i64 }
-- output { 499999500000i64 }

def main (n: i64) : i64 =
  reduce (+) 0 (iota n)
