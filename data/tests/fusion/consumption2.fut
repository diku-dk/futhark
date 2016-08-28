-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Redomap 1 }

fun main(as: [n]int, bs: [m]bool): [n]int =
  let css = map (fn (b: bool): [n]int  =>
                  if b then iota(n) else as) bs
  let dss = map  (fn (cs: *[n]int): [n]int  =>
                   let cs[0] = 42
                   in cs) css
  in reduce (fn (ds0: []int) (ds1: []int): [n]int  =>
              zipWith (+) ds0 ds1) (
            replicate n 0) dss
