-- Record field access can be nested.
-- ==
-- input { 2 } output { 3 }

def main (x: i32) =
  let r = {a = x, b = x + 1, c = {b = x, a = {a = {b = x + 1}}}}
  in r.c.a.a.b
