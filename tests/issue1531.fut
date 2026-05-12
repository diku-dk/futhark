type~ t0 = ?[n][m].([n]i64, [m]i64)

type~ t1 = (t0, t0)

def main : t1 =
  let a = (iota 1, iota 2)
  let b = (iota 3, iota 4)
  in (a, b)
