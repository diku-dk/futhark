-- ==
-- input {
-- }
-- output {
--   0i64
-- }
def f (a: *[]i64) = a[0]
def g (a: []i64) = a[0]

def main : i64 =
  let n = 10
  let a = iota (n)
  let b = a
  in if 1 == 2
     then let c = g (b) in f (a) + c
     else let c = g (a) in f (b)

-- OK Because only one branch is taken.
