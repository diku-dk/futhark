-- After fusion, consumes a free variable.  Fixed with copy().
--
-- ==
-- structure { /Screma 1 }

def main [n] [m] (as: [n]i32, bs: [m]bool) : [m][n]i32 =
  let css =
    map (\(b: bool) : [n]i32 ->
           if b then map (+ 1) as else as)
        bs
  let dss =
    map (\(cs: [n]i32) : [n]i32 ->
           copy cs with [0] = 42)
        css
  in dss
