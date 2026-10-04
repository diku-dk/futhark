-- Updating one field of a consumed record and returning it as fresh.
-- ==
-- input { [1] [2] }
-- output { [2] [2] }

type~ state = {a: []i32, b: []i32}

def step (s: *state) : *state = s with a = map (+ 1) s.a

def main (xs: *[]i32) (ys: *[]i32) =
  let s = step {a = xs, b = ys}
  in (s.a, s.b)
