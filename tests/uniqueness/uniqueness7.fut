-- ==
-- input {
-- }
-- output {
--   0i64
-- }
def f (a: *[][]i64) = a[0, 0]

def main : i64 =
  let n = 10
  let a = replicate n (iota n)
  let b = replicate n (iota n)
  let a =
    loop (a) for i < n do
      let a[i] = b[i] in a
  -- Does not alias a to b, because let-with is in-place!

  let x = f (b)
  -- Consumes only b.
  in a[0, 0]

-- OK
