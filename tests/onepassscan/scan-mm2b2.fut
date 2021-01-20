-- Simple intra-group scan
-- ==
-- compiled random input { [100000]i32 } auto output

let main [n] (arr : [n]i32) : ([n]i32, [n]i32, [n]i32, [n]i32) =
  let mm2b2 (a1, b1, c1, d1) (a2, b2, c2, d2) =
    ( a1*a2 + b1*c2
    , a1*b2 + b1*d2
    , c1*a2 + d1*c2
    , c1*b2 + d1*d2 
    )
  in map (\i -> (i/11 + 1, i%11 + 1, i/37 + 1, i%37 + 1)) arr
  |> scan mm2b2 (1i32, 0i32, 0i32, 1i32) |> unzip4
