-- ==
fun main(a0: []f64, a1: []int, oks: []bool): []f64 =
  let (b, _) = unzip(filter (fn (x: (f64,int)): bool  =>
                              let (_,i) = x in
                              unsafe oks[i]) (
                            zip(a0,a1))) in
  b
