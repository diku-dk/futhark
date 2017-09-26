-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Redomap 1 }

let main [n][m] (as: [n]i32, bs: [m]bool): [n]i32 =
  let css = map (\(b: bool): [n]i32  ->
                  if b then iota(n) else as) bs
  let dss = map  (\(cs: *[]i32): [n]i32  ->
                   let cs[0] = 42
                   in cs) css
  in reduce (\(ds0: []i32) (ds1: []i32): [n]i32  ->
              map (+) ds0 ds1) (
            replicate n 0) dss
