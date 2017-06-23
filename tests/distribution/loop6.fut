-- Interchange of a loop where some parts are dead after the loop.
-- ==
-- structure distributed { /Kernel 0 /DoLoop 1 /DoLoop/Kernel 1 }

let main [m] [n] (xss: *[m][n]i32) =
  map (\xs ->
       #2 (loop ((xs,out) = (xs, replicate n 0f32)) for i < n do
         (let xs = map (+1) xs
          let out = map (+) (map f32 xs) out
          in (xs, out)))
      ) xss
