-- Type ascription should not hide aliases.
-- ==
-- error: "a".*consumed

def main(): i64 =
  let a = iota(10)
  let b:*[]i64 = a
  let b[0] = 1
  in a[0]
