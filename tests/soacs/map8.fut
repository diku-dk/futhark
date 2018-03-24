-- Test whether the internaliser can properly handle very complex map
-- return types.
-- ==

let main(a1: []i32, a2: []i32): i32 =
  let b = map2 (\(x: i32) (y: i32): ([](i32,i32),[](i32,i32))  ->
                    (zip (iota(x)) (iota(y)),zip (iota(x)) (iota(y)))) a1 a2 in
  0
