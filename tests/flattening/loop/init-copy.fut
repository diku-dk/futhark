-- A map of a loop that consumes its initial values (a double-buffered
-- scatter loop, as in an FFT). The copies that feed the consuming
-- loop must reside inside the fully flattened code version, not
-- before the versioning branch, where they would also be executed
-- when another version is picked at runtime.
-- ==
-- input { [[1i64, 2i64], [3i64, 4i64]] }
-- output { [[4i64, 5i64], [6i64, 7i64]] }
-- structure gpu { /Replicate 0 /If/False/Replicate 2 }

def main [n] [m] (xss: [n][m]i64) : [n][m]i64 =
  map (\xs ->
         let (res, _) =
           loop (inp, out) = (copy xs, copy xs)
           for _i < 3 do
             (scatter out (iota m) (map (+ 1) inp), inp)
         in res)
      xss
