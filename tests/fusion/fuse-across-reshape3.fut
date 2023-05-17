-- structure { Map 3 Map/Map/Map 1 Map/Map/Scan 1 }

def main(n: i64, m: i64, k: i64): [][][]f32 =
  map (\(ar: [][]f32): [m][n]f32  ->
        map (\(arr: []f32): [n]f32  ->
              scan (+) 0f32 arr) ar) (
      unflatten_3d (map f32.i64 (iota(k*m*n))))
