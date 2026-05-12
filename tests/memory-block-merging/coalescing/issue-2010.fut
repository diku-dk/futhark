-- partition2.fut
-- ==
-- input { [1i32,2i32,3i32,4i32,5i32,6i32,7i32]
-- }
-- output {
--   3i64
--   [2i32,4i32,6i32,1i32,3i32,5i32,7i32]
-- }

def partition2 't [n]
               (dummy: t)
               (cond: t -> bool)
               (X: [n]t) : (i64, *[n]t) =
  let cs = map cond X
  let tfs =
    map (\f ->
           if f
           then 1i64
           else 0i64)
        cs
  let isT = scan (+) 0 tfs
  let i = isT[n - 1]
  let ffs =
    map (\f ->
           if f
           then 0
           else 1)
        cs
  let isF = map (+ i) <| scan (+) 0 ffs
  let inds =
    map (\(c, iT, iF) ->
           if c
           then iT - 1
           else iF - 1)
        (zip3 cs isT isF)
  let tmp = replicate n dummy
  in (i, scatter tmp inds X)

def main [n] (arr: *[n]i32) : (i64, *[n]i32) =
  partition2 0i32 (\(x: i32) -> (x & 1i32) == 0i32) arr
