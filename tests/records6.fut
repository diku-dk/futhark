-- It is OK to define the same field twice, although only the last one
-- takes effect.  This should result in a warning, but we don't/can't
-- test that right now.
--
-- ==
-- input { 2 } output { 4 3 }

let main(x: i32) =
  let r = {a=x, b=x+1, a=x+2}
  in (r.a, r.b)
