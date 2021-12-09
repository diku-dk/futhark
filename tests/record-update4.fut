-- Chained record update in local function.
-- ==
-- input { 0 1 } output { 0 1 }

type record = {x: i32, y: i32}

def main (x: i32) (y: i32) =
  let f b (r: record) = if b then r else r with x = 1 with y = 2
  let r' = f true {x,y}
  in (r'.x, r'.y)
