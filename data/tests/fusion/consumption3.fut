-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Scanomap 1 }

fun [m][n]int main([n]int as, [m]bool bs) =
  let css = map(fn [n]int (bool b) =>
                  if b then iota(n) else as,
                bs)
  let dss = map (fn [n]int (*[n]int cs) =>
                   let cs[0] = 42
                   in cs,
                 css)
  in scan(fn [n]int (*[]int ds0, *[]int ds1) =>
            loop (ds0) = for i < n do
              let ds0[i] = ds0[i] + ds1[i]
              in ds0
            in ds0,
          replicate(n, 0), dss)
