-- Type-checking the body may induce extra constraints on the
-- parameters that makes the return type invalid wrt. the parameter.
--
-- Based on issue 1565.
-- ==
-- error: Loop body does not have expected type

def main [n] (xs: [n]i32) (ys: [n]i32) =
  loop (xs, ys) for i < 10 do
    let zs = filter (> 0) (map2 (+) xs ys)
    in (xs, zs)
