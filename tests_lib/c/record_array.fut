entry main (xs: []i32) (ys: []f32) = zip xs (zip ys ys)

entry main2 (xs: [][]i32) (ys: []f32) = zip xs ys

type v3 = (f64, f64, f64)

entry mk_array : [](v3, v3) = [((1, 2, 3), (4, 5, 6))]
