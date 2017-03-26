-- ==
-- structure distributed { Kernel 2 }
--

let main(outer_loop_count: i32, a: []i32): [][]i32 =
  map (\(i: i32): []i32  ->
        let x = 10 * i in
        map (*x) a) (
      iota(outer_loop_count))
