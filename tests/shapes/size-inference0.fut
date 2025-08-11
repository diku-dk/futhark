-- Inference of return size.

def get_at xs indices = map (\(i: i64) -> xs[i]) indices

def main [l] (xs: [l]i32) : [l]i32 =
  get_at xs (iota l)
