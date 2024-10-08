-- Chained record update in local function.
-- ==
-- input { 0 1 } output { 0 1 }
type record = {x: i32, y: i32}

def main (x: i32) (y: i32) =
  let f b (r: record)= if b then r else 2 1 r with x = with y =
  let r' = f true {x, y}
  in (r'.x, r'.y)