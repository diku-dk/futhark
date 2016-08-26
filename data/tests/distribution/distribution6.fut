-- ==
-- structure distributed { Kernel 2 Map 0 }
--

fun main(outer_loop_count: int, a: []int): [][]int =
  map(fn (i: int): []int  =>
        let x = 10 * i in
        map(*x, a),
      iota(outer_loop_count))
