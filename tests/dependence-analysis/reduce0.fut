-- ==
-- structure { BinOp 2 }

def plus (a,b) (x,y): (i32,i32) = (a+x,b+y)

def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop r' = (0,0) for i < n do
      reduce (\(a,b) (x,y) -> if i == 0 then (a,b) else (a+x,b+y))
             (0,0)
             (map (plus r') A)
  in r.0
