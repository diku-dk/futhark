-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { /Screma 1 }

def main [n] [m] (as: [n]i32, bs: [m]bool) : [n]i32 =
  let css =
    map (\(b: bool) : [n]i32 ->
           if b then map i32.i64 (iota n) else as)
        bs
  let dss =
    map (\(cs: []i32) : [n]i32 ->
           copy cs with [0] = 42)
        css
  in reduce (\(ds0: []i32) (ds1: []i32) : [n]i32 ->
               map2 (+) ds0 ds1)
            (replicate n 0)
            dss
