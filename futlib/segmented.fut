-- | Segmented operations.

-- | Segmented scan. Given a binary associative operator ``op`` with
-- neutral element ``ne``, compute the inclusive prefix scan of the
-- segments of ``as`` specified by the ``flags`` array, where `true`
-- starts a segment and `false` continues a segment.
let segmented_scan [n] 't (op: t -> t -> t) (ne: t)
                          (flags: [n]bool) (as: [n]t): [n]t =
  (unzip (scan (\(x_flag,x) (y_flag,y) ->
                (x_flag || y_flag,
                 if y_flag then y else x `op` y))
          (false, ne)
          (zip flags as))).2
