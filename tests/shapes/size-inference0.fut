-- Inference of return size.

let get_at xs indices = map (\i -> xs[i]) indices

let main [l] (xs: [l]i32): [l]i32 =
  get_at xs (iota l)
