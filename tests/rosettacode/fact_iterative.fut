-- ==
-- input {
--   10
-- }
-- output {
--   3628800
-- }
def fact (n: i32) : i32 =
  loop out = 1
  for i < n do
    out * (i + 1)

def main (n: i32) : i32 =
  fact (n)
