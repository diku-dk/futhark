-- ==
-- input {
--   42i64
-- }
-- output {
--   [1.000000f32]
--   [2.000000f32]
-- }
def f (b_1: *[]i64) : ([]f32, []f32) =
  ([1.0], [2.0])

def main (n: i64) : ([]f32, []f32) =
  let a = iota (n)
  let x = f (a)
  in x
