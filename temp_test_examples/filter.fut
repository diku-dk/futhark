def filter' [n] 'a (p: a -> bool) (as: [n]a) : *[]a =
  if n == 0
  then []
  else
    let flags = map (\x -> if p x then 1 else 0) as
    let offsets = scan (+) 0 flags
    let result =
      scatter (map (\_ -> as[0]) (0..1..<n))
              (map2 (\f o -> if f==1 then o-1 else -1) flags offsets)
              as
    in result[:offsets[n - 1]]


entry main [n] (as: [n]i32): *[]i32 =
  filter' ((i32.% 2) >-> bool.i32) as

-- ==
-- entry: test
-- random input { [10000]i32 }
-- output { true }
entry test [n] (as: [n]i32): bool =
  main as |> all i32.((==1) <-< (%2))
  
