-- No scan with consuming operator!
-- ==
-- error:

let main(a: *[]i32,b: []i32): i32 =
  let c =
    scan (\(acc: (*[]i32, []i32)) (i: ([]i32, []i32)): (*[]i32, []i32)  ->
             let (a2,b2) = acc in (a2,b2)) (a,b) (zip (replicate 10 (iota 10)) (
                      replicate 10 (iota 10))) in
  length c + length b -- Should be OK, because only a has been
                      -- consumed.
