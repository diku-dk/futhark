entry main [n] (as: [n]i32): *[]i32 =
  if n != 0 then
    let flags = map (\x -> if x i32.% 2 |> bool.i32 then 1 else 0) as
    let offsets = scan (+) 0 flags
    let result =
      scatter (replicate n 0)
            (map2 (\f o -> if f==1 then o-1 else -1) flags offsets)
            as
    in result[:last offsets]
  else []
