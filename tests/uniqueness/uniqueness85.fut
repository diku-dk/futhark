-- A loop evaluates its initial value before its form, so the form may consume
-- what the initial value used.
-- ==
-- input { [5, 6, 7] } output { 19 }

def main (ys: *[]i32) =
  loop acc = ys[0] for x in (let zs = ys with [0] = 1 in zs) do acc + x
