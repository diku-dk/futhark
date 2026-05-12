-- ==
-- input {
-- }
-- output {
--   0
-- }
def main : i32 =
  let n = 10
  let a = iota (n)
  let b = iota (n)
  let c = (a, b)
  let (a_, unused_b) = (a, b)
  let a[0] = 0
  -- Only a_ and a are consumed.
  let (unused_a, b_) = (a, b)
  let b[0] = 1
  -- Only b_ and b are consumed.
  in 0
