-- ==
fun [f64] main([(f64,int)] a, [bool] oks) =
  let (b, _) = unzip(filter(fn bool ((f64,int) x) =>
                              let (_,i) = x in
                              unsafe oks[i],
                            a)) in
  b
