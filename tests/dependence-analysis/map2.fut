def main [d] (A: [d](i64,i64)) (n: i64) =
  let r =
    loop A for i < n do
      map (\(a,b) -> let y1 = a+i
                     let y2 = y1+1
                     let y3 = y2+d
                     in (y3,b)
          ) A
  in map (.0) r
