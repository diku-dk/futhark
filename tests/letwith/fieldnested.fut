-- ==
-- input { 42 } output { 42 }
def main (x: i32) : i32 =
  let rec = {inner = {x = 0}}
  let rec.inner.x = x
  in rec.inner.x