-- Nonsense, but ought to type-check.
def main (xs: [10]i64) =
  loop xs for i < 10 do
    let n = 10 + i
    in (xs :> [n]i64) ++ [i]
