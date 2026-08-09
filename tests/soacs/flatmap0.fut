-- Basic flatmap: expand each element into 'k' copies of itself, and check that
-- the data and all the metadata arrays are correct, as well as the value result.
-- ==
-- input { [0i64, 2i64, 3i64, 1i64] [5i32, 10i32, 20i32, 30i32] }
-- output { [0i64, 2i64, 3i64, 1i64]
--          [true, false, true, false, false, true]
--          [0i64, 0i64, 2i64, 5i64]
--          [10i32, 10i32, 20i32, 20i32, 20i32, 30i32]
--          [10i32, 20i32, 40i32, 60i32] }
-- input { [2i64, 3i64, 1i64] [10i32, 20i32, 30i32] }
-- output { [2i64, 3i64, 1i64]
--          [true, false, true, false, false, true]
--          [0i64, 2i64, 5i64]
--          [10i32, 10i32, 20i32, 20i32, 20i32, 30i32]
--          [20i32, 40i32, 60i32] }
-- input { empty([0]i64) empty([0]i32) }
-- output { empty([0]i64)
--          empty([0]bool)
--          empty([0]i64)
--          empty([0]i32)
--          empty([0]i32) }

def main (ks: []i64) (xs: []i32) =
  let f (k: i64, x: i32): ?[m].([m]i32, i32) = (replicate k x, x * 2)
  in flatmap f (zip ks xs)
