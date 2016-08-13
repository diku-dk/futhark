-- ==
fun []f64 main([]f64 a0, []int a1, []bool oks) =
  let (b, _) = unzip(filter(fn bool ((f64,int) x) =>
                              let (_,i) = x in
                              unsafe oks[i],
                            zip(a0,a1))) in
  b
