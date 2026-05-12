-- ==
-- input { [0i64,0i64] [[1f32,2f32],[3f32,4f32]] [[0f32,0f32]] }
-- output { [[3f32,4f32]] [[1i64,1i64]] }

def argmax (x: f32, i: i64) (y: f32, j: i64) =
  if x == y
  then (x, i64.max i j)
  else if x > y
  then (x, i)
  else (y, j)

def main [n] [m] [k] (is: [n]i64) (vs: [n][k]f32) (dst: [m][k]f32) =
  let dst_cpy = copy dst
  let res =
    reduce_by_index (map2 zip dst_cpy (replicate m (replicate k (-1))))
                    (map2 argmax)
                    (replicate k (f32.lowest, -1))
                    is
                    (map2 zip vs (map (replicate k) (iota n)))
  in unzip (map unzip res)
