def windows k s =
  map (\i -> take k (drop i s)) (take (length s - k) (indices s))

entry main (s: []i32) = windows 14 s
