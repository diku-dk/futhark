-- Test that attributes don't disappear after fusion.
-- ==
-- structure { Screma 1 }
-- structure gpu { /SegMap 0 /DoLoop 1 }

let main (xs: []i32) =
  (#[sequential] map (+1) xs, map (*2) xs)
