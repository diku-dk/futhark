-- Returning an existential type in a branch that constructs a new
-- value referencing the sizes of the other branch should be
-- permitted, as the return type hides the size.
-- ==
-- input { true } output { 4i64 }
-- input { false } output { 3i64 }

type~ outer = { arr: []i32, seed: i32 }

def event (b: bool) (s: outer) : outer =
  if b
  then {arr = replicate (length s.arr + 1) 0, seed = s.seed}
  else s

entry main (b: bool) = (event b {arr = [1,2,3], seed = 42}).arr |> length
