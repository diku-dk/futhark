-- ==
-- compiled input { 6400000i64 } output { 30483u16 38172u16 }

let min_max_nonzero (a: (u16, u16)) (b: (u16, u16)): (u16, u16) =
  let (amin, amax) = a
  let (bmin, bmax) = b
  let min =
    if amin == 0 then bmin
    else if bmin == 0 then amin
    else u16.min amin bmin
  in (min, u16.max amax bmax)

entry main (n: i64): (u16, u16) =
  let values =
    map
      (\i ->
        if i == 0 then 30483u16
        else if i == n - 1 then 38172u16
        else 32000u16)
      (iota n)
  in reduce_comm min_max_nonzero (0u16, 0u16) (map (\x -> (x, x)) values)
