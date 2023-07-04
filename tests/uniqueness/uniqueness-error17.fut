-- Test that aliasing is found, even if hidden inside a
-- branch.
-- ==
-- error: .*consumed.*

def main n =
  let a = iota(n)
  let c = if 2==2 then iota(n) else a -- c aliases a.
  let c[0] = 4 in -- Consume c and a.
  a[0] -- Error, because a was consumed.
