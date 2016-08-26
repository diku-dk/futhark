-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Scanomap 1 }

fun main(as: [n]int, bs: [m]bool): [m][n]int =
  let css = map(fn (b: bool): [n]int  =>
                  if b then iota(n) else as,
                bs)
  let dss = map (fn (cs: *[n]int): [n]int  =>
                   let cs[0] = 42
                   in cs,
                 css)
  in scan(fn (ds0: *[]int, ds1: *[]int): [n]int  =>
            loop (ds0) = for i < n do
              let ds0[i] = ds0[i] + ds1[i]
              in ds0
            in ds0,
          replicate(n, 0), dss)
