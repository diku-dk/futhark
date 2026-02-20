-- ==
-- input { 9i32 } output { 9i32 }

def main (y: i32) : i32 =
  let xs = [{f=1i32}, {f=2i32}]
  let xs[1].f = y
     in xs[1].f