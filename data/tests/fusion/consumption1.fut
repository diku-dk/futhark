-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Map 1 }

fun main(as: [n]int, bs: [m]bool): [m][n]int =
  let css = map (fn (b: bool): [n]int  =>
                  if b then iota(n) else as) bs
  let dss = map  (fn (cs: *[n]int): [n]int  =>
                   let cs[0] = 42
                   in cs) css
  in dss
