entry main (x: i32): (i32, bool) =
  let when pred action orig = if pred
                              then action orig
                              else orig

  let action = when true (\(x, _) -> (x, true))

  in if true
     then action (x, false)
     else action (x, false)
