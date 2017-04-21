-- Can't mix explicit and implicit shape quantification.
-- ==
-- error: explicit

let main [n] (x: [n]i32) (y: [#m]i32) = n + m
