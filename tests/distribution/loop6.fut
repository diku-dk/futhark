-- Interchange of a loop where some parts are dead after the loop.
-- ==
-- structure gpu { /SegMap 0 /Loop 1 /Loop/SegMap 1 }

def main [m] [n] (xss: *[m][n]i64) =
  #[incremental_flattening(only_inner)]
  map (\xs ->
       (loop (xs,out) = (xs, replicate n 0f32) for i < n do
         (let xs = map (+1) xs
          let out = map2 (+) (map f32.i64 xs) out
          in (xs, out))).1
      ) xss
