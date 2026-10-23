-- ==
def main (a0: []f32, a1: []i32, oks: []bool) : []f32 =
  let (b, _) =
    unzip (filter (\(x: (f32, i32)) : bool ->
                     let (_, i) = x
                     in oks[i])
                  (zip a0 a1))
  in b
