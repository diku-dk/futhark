-- ==
-- input { 7 } output { 7 2 }
def main (x: i32) : (i32, i32) =
  let rec = {a = 1, b = 2}
  let rec.a = x
  in (rec.a, rec.b)
