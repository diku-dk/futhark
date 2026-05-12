-- What about size parameters that are only known in complex expressions?
-- ==
-- input { [1,2] [3,4] }
-- output { [3,4,1,2] }

type eq [n] [m] = [n][m]()
def coerce [n] [m] 't (_: eq [n] [m]) (a: [n]t) = a :> [m]t
def plus_comm [a] [b] 't : eq [a + b] [b + a] = tabulate_2d (a + b) (b + a) (\_ _ -> ())

def main [n] [m] (xs: [n]i32) (ys: [m]i32) =
  copy (coerce plus_comm (ys ++ xs))
