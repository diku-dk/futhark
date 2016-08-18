-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Map 1 }

fun [m][n]int main([n]int as, [m]bool bs) =
  let css = map(fn [n]int (bool b) =>
                  if b then iota(n) else as,
                bs)
  let dss = map (fn [n]int (*[n]int cs) =>
                   let cs[0] = 42
                   in cs,
                 css)
  in dss
