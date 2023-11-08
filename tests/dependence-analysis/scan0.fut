-- ==
-- structure { BinOp 1 }

def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop A for i < n do
      scan (\(a,b) (x,y) -> if i == 0 then (a,b) else (a+x,b+y)) (0,0) A
  in map (.0) r
