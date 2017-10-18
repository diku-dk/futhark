-- If the reduction function accumulator type is unique, consume the
-- initial value, but only as much as is actually unique!
-- ==
-- input {
--   [0,1,2,3,4,5,6,7,8,9]
--   [9,8,7,6,5,4,3,2,1,0]
-- }
-- output {
--   20
-- }

let main(a: *[]i32,b: []i32): i32 =
  let c =
    scan (\(acc: (*[]i32, []i32)) (i: ([]i32, []i32)): (*[]i32, []i32)  ->
             let (a2,b2) = acc in (a2,b2)) (a,b) (zip (replicate 10 (iota 10)) (
                      replicate 10 (iota 10))) in
  (shape c)[0] + (shape b)[0] -- Should be OK, because only a has been
                              -- consumed.
