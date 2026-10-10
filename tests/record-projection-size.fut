-- A size that is a projection of a record parameter (g.n) must be
-- substituted when the record binding is replaced by a record pattern,
-- also when it occurs inside another function's inferred return type.
-- ==
-- input { 2i64 }
-- output { [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32] }

type geo = {n: i64}

def tq (g: geo) = {a = replicate g.n 0f32}

def ts (g: geo) : ([]f32, []f32) = (replicate (g.n * 2) 0, replicate (g.n * 3) 0)

entry main (n: i64) = let g = {n} in (tq g).a ++ (ts g).1
