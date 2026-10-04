-- A local function declared to return a fresh value may not return an outer
-- local.
-- ==
-- error: not consumable

def main (n: i64) : i32 =
  let x = replicate n 0i32
  let f (_: i32): *[]i32 = x
  let r = f 0
  let r[0] = 10
  in x[0]
