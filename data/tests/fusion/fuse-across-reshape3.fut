-- structure { Map 3 Map/Map/Map 1 Map/Map/Scan 1 }

fun main(n: int, m: int, k: int): [][][]f32 =
  map(fn (ar: [][]f32): [m][n]f32  =>
        map(fn (arr: []f32): [n]f32  =>
              scan((+), 0f32, arr),
            ar),
      reshape (k,m,n) (map(f32, iota(n*m*k))))
