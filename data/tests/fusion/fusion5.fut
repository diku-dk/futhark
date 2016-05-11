-- Once failed in fusion.  Derived from tail2futhark output.
-- ==
-- input { [1, 2, -4, 1] [[1, 2], [-4, 1]] }
-- output {
--          [[True, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False],
--           [True, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False, False, False, False,
--            False, False, False, False, False, False, False, False]]
-- }
-- structure { Map 3 Map/Map 1 }
fun [[bool]] main([int] t_v1, [[int]] t_v3) =
  let n = 3 in
  let t_v6 = map(fn int (int x) => (x + 1),iota(n)) in
  let t_v12 = map(fn int (int x) => (x + 1),iota(30)) in
  let t_v18 = rearrange((1,0),replicate(30, t_v6)) in
  let t_v19 = replicate(n, t_v12) in
  let t_v27 = map(fn [int] ([int] x,[int] y) =>
                    map(^,zip(x,y)),
                  zip(t_v18,
                      map(fn [int] ([int] x) => map(<<1, x), t_v18))) in
  let t_v33 = map(fn [bool] ([int] x) =>
                    map(fn bool (int t_v32) =>
                          ((0 != t_v32)),x),
                    map(fn [int] ([int] x,[int] y) =>
                          map(&,zip(x,y)),
                        zip(t_v27,
                            map(fn [int] ([int] x) =>
                                  map(fn int (int t_v29) =>
                                        (1 >> t_v29),x),
                                  map(fn [int] ([int] x) =>
                                        map(fn int (int t_v28) =>
                                              (t_v28 - 1),
                                            x),
                                        t_v19))))) in
  t_v33
