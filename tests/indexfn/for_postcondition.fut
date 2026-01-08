def f1 [n]
  (shape: {[n]i64 | \x -> Range x (0,inf)})
  (cs: [n]bool)
  : { [n]i64 | \ys -> For ys (\i -> Range ys (0,shape[i])) }
  =
  map2 (\c x -> if c then x else 0) cs shape

def f2
  (n: {i64 | \x -> Range x (0,inf)})
  (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
  : { [n]i64 | \ys -> For ys (\i -> Range ys (0,offsets[i+1])) }
  =
  -- Should be equivalent to f3, algebraically.
  let sum0 = scan (\x y -> x + y) 0 offsets
  let sum1 = map (\x -> x - offsets[0]) sum0
  in map (\i -> sum1[i+1] - sum0[i]) (iota n)

def f2_conditional
  (n: {i64 | \x -> Range x (0,inf)})
  (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
  (cs: [n]bool)
  : { [n]i64 | \ys -> For ys (\i -> Range ys (0,offsets[i+1])) }
  =
  -- Should be equivalent to f3, algebraically.
  let sum0 = scan (\x y -> x + y) 0 offsets
  let sum1 = map (\x -> x - offsets[0]) sum0
  in map (\i -> if cs[i] then sum1[i+1] - sum0[i] else 0) (iota n)


-- def f3
--   (n: {i64 | \x -> Range x (0,inf)})
--   (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
--   : { [n]i64 | \ys -> For ys (\i -> Range ys (0,offsets[i+1])) }
--   =
--   let shape = map (\i -> offsets[i+1] - offsets[i]) (iota n)
--   in scan (\x y -> x + y) 0 shape

-- def f3_conditional
--   (n: {i64 | \x -> Range x (0,inf)})
--   (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
--   (new: [n]bool)
--   : { [n]i64 | \ys -> For ys (\i -> Range ys (0,offsets[i+1])) }
--   =
--   let shape = map (\i ->
--     if new[i] then offsets[i+1] - offsets[i] else 0
--   ) (iota n)
--   in scan (\x y -> x + y) 0 shape

-- PLAN
--
-- * return to mis.fut; revert to only using offsets
-- * make f3 validate
-- * define f3_conditional
-- * make f3_conditional validate




-- def mk_indices
--   (n: {i64 | \x -> Range x (0,inf)})
--   (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
--   : { [n]i64 | \y -> InjectiveRCD y (0,inf)}
--   =
--   map (\i ->
--     if offsets[i+1] - offsets[i] > 0 then offsets[i] else -1
--   ) (iota n)
-- def mk_indices
--   (n: {i64 | \x -> Range x (0,inf)})
--   (shape: {[n+1]i64 | \x -> Range x (0,inf)})
--   : { [n]i64 | \y -> InjectiveRCD y (0,inf)}
--   =
--   map (\i ->
--     if shape[i+1] - shape[i] > 0 then shape[i] else -1
--   ) (iota n)

def f4
  (n: {i64 | \x -> Range x (0,inf)})
  (offsets: {[n+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
  : { [n]i64 | \_ -> true }
  =
  -- can we scatter using offsets?
  let idxs = map (\i ->
    if offsets[i+1] - offsets[i] > 0 then offsets[i] else -1
  ) (iota n)

  in scatter (replicate n 0) idxs idxs



-- def main [n] (offsets: {[n]i64 | \x -> Range x (0,inf) && Monotonic (<=) x}) : { [n-1]i64 | \ys -> For ys (\i -> Range ys (0,offsets[i+1])) } =
--   let shape = map (\i -> offsets[i+1] - offsets[i]) (iota (n - 1))
--   in scan (\x y -> x + y) 0 shape
