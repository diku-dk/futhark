-- No reduction with consuming operator!
-- ==
-- error:

let main(a: *[]i32,b: []i32): []i32 =
  let (x,y) =
    reduce (\(acc: (*[]i32, []i32)) (arr: ([]i32, []i32)): (*[]i32, []i32) ->
             let (a1,b1) = acc
             let (a2,b2) = arr
             in (map2 (+) a1 a2,
                 map2 (*) b1 b2))
           (a,b) (zip (replicate 10 (iota 10)) (replicate 10 (iota 10)))
  in map2 (+) b x -- Should be OK, because only a has been consumed.
