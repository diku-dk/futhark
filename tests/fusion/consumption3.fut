-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { Scanomap 1 }

let main(as: [#n]i32, bs: [#m]bool): [m][n]i32 =
  let css = map (\(b: bool): [n]i32  ->
                  if b then iota(n) else as) bs
  let dss = map  (\(cs: *[#n]i32): [n]i32  ->
                   let cs[0] = 42
                   in cs) css
  in scan (\(ds0: *[]i32) (ds1: *[]i32): [n]i32  ->
            loop (ds0) = for i < n do
              let ds0[i] = ds0[i] + ds1[i]
              in ds0
            in ds0) (
          replicate n 0) dss
