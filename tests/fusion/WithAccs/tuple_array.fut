-- This currently cannot be fused, but at one point it crashed fusion.
-- ==
-- structure { WithAcc 2 }

import "../../accs/intrinsics"

def op (x1: i32, y1: i32) (x2: i32, y2: i32) =
  (x1 + x2, y1 + y2)

entry main [k] [n] (dest: *[k](i32, i32)) (is1: [n]i64) (vs1: [n](i32, i32)) (is2: [n]i64) (vs2: [n](i32, i32)) =
  let tmp =
    reduce_by_index_stream dest
                           op
                           (0, 0)
                           (\(acc: *acc ([](i32, i32))) (i, v) ->
                              write acc i v)
                           (zip is1 vs1)
  in reduce_by_index_stream tmp
                            op
                            (0, 0)
                            (\(acc: *acc ([](i32, i32))) (i, v) ->
                               write acc i v)
                            (zip is2 vs2)
