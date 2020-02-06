-- Once failed in fusion.  Derived from tail2futhark output.
-- ==
-- input { [1, 2, -4, 1] [[1, 2], [-4, 1]] }
-- output {
--          [[true, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false],
--           [false, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false],
--           [true, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false, false, false, false,
--            false, false, false, false, false, false, false, false]]
-- }
-- structure { /Screma 3 /Screma/Screma 1 }
let main(t_v1: []i32) (t_v3: [][]i32): [][]bool =
  let n = 3
  let t_v6 = map (\(x: i32): i32  -> (x + 1)) (iota(n))
  let t_v12 = map (\(x: i32): i32  -> (x + 1)) (iota(30))
  let t_v18 = transpose (replicate 30 t_v6)
  let t_v19 = replicate n t_v12
  let t_v27 = map (\(x: []i32,y: []i32)  ->
                    map2 (^) x y) (
                  zip (t_v18) (
                      map (\(x: []i32)  -> map (<<1) x) (t_v18)))
  let t_v33 = map (\(x: []i32)  ->
                    map (\(t_v32: i32): bool  ->
                          ((0 != t_v32))) x) (
                    map (\(x: []i32,y: []i32)  ->
                          map2 (&) x y) (
                        zip (t_v27) (
                            map (\(x: []i32)  ->
                                  map (\(t_v29: i32): i32  ->
                                        (1 >> t_v29)) x) (
                                  map (\(x: []i32)  ->
                                        map (\(t_v28: i32): i32  ->
                                              (t_v28 - 1)) x) (
                                        t_v19))))) in
  t_v33
