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
fun main(t_v1: []int, t_v3: [][]int): [][]bool =
  let n = 3 in
  let t_v6 = map(fn (x: int): int  => (x + 1),iota(n)) in
  let t_v12 = map(fn (x: int): int  => (x + 1),iota(30)) in
  let t_v18 = rearrange((1,0),replicate(30, t_v6)) in
  let t_v19 = replicate(n, t_v12) in
  let t_v27 = map(fn (x: []int,y: []int): []int  =>
                    map(^,zip(x,y)),
                  zip(t_v18,
                      map(fn (x: []int): []int  => map(<<1, x), t_v18))) in
  let t_v33 = map(fn (x: []int): []bool  =>
                    map(fn (t_v32: int): bool  =>
                          ((0 != t_v32)),x),
                    map(fn (x: []int,y: []int): []int  =>
                          map(&,zip(x,y)),
                        zip(t_v27,
                            map(fn (x: []int): []int  =>
                                  map(fn (t_v29: int): int  =>
                                        (1 >> t_v29),x),
                                  map(fn (x: []int): []int  =>
                                        map(fn (t_v28: int): int  =>
                                              (t_v28 - 1),
                                            x),
                                        t_v19))))) in
  t_v33
