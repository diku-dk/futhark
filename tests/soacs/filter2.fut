-- ==
def main (a0: []f64, a1: []i32, oks: []bool) : []f64 =
  let (b, _) =
    unzip (filter (\(x: (f64, i32)) : bool ->
                     let (_, i) = x
                     in oks[i])
                  (zip a0 a1))
  in b
