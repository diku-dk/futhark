-- ==
-- input { [0f32, 1f32, 2f32, 3f32, 4f32, 5f32, 6f32, 7f32, 8f32,
-- 9f32, 10f32, 11f32, 12f32, 13f32, 14f32, 15f32, 16f32, 17f32,
-- 18f32, 19f32] }
-- output { [0f32, 1f32, 3f32, 4f32, 6f32, 7f32, 9f32, 10f32, 12f32,
-- 13f32, 15f32, 16f32, 18f32, 19f32, 21f32, 22f32, 24f32, 25f32,
-- 27f32, 28f32] }

import "intrinsics"

def f (acc: *acc ([]f32)) i =
  let acc = write acc (i * 2) (f32.i64 i)
  let acc = write acc (i * 2 + 1) (f32.i64 i)
  in acc

def main (xs: *[]f32) =
  reduce_by_index_stream xs (+) 0 f (iota 10)
