-- Segmented operations using parametric modules.

import "futlib/monoid"

module segmented_scan(M: monoid): {
  val segmented_scan: []bool -> []M.t -> []M.t
} = {
  let segmented_scan(flags: [n]bool) (as: [n]M.t): []M.t =
    #2 (unzip (scan (\(x_flag,x) (y_flag,y) ->
                     if y_flag
                     then (x_flag || y_flag, y)
                     else (x_flag || y_flag, M.op x y))
                    (false, M.ne)
                    (zip flags as)))
}
