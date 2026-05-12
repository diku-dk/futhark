-- Inference of return size (which then causes a type error).
-- ==
-- error: "10" and "l" do not match

def get_at xs indices = map (\(i: i64) -> xs[i]) indices

def main [l] (xs: [l]i32) : [10]i32 =
  get_at xs (iota l)
