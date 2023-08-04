def main (A: [](i64,i64)) (n: i64) =
  let r =
    loop A for i < n do
      map (\(a,b) -> let y = a+i in (y,b)) A
  in map (.0) r
