// --
// structure distributed { Map 3 }
//

fun [[int]] main(int outer_loop_count, [int] a) =
  map(fn [int] (int i) =>
        let x = 10 * i in
        map(*x, a),
      iota(outer_loop_count))
