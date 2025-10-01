def segment_sum [n] 't
      -- (flags: {[n]bool | \x -> x[0] == true})
      (flags: [n]bool)
      (xs: [n]i64) : {[n]i64 | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys
