-- The map should be simplified away entirely, even though it is a
-- call to a built-in function.
-- ==
-- structure distributed { Kernel 1 }

let main (n: i32) (accs: []i32) =
  let ys = map (2**) (iota n)
  in map (\(acc:i32) -> loop acc for y in ys do acc * y) accs
