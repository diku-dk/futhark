-- structure { Map 3 Map/Map/Map 1 Map/Map/Scan 1 }

fun [[[f32]]] main(int n, int m, int k) =
  map(fn [[f32,n],m] ([[f32]] ar) =>
        map(fn [f32,n] ([f32] arr) =>
              scan(+, 0f32, arr),
            ar),
      reshape((k,m,n), map(f32, iota(n*m*k))))
