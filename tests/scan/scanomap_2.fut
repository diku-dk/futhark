-- ==
-- random input { [384]i32 [384]i32 [384]i32 } auto output
let main [n] (x : [n]i32) (y: [n]i32) (z: [n]i32)=
    let (x',y') = unzip <| map2 (\x y -> (x+y, x-y)) x y
    let z' = map (*2) z
    let x'' = scan (+) 0i32 x'
    let y'' = scan i32.max i32.lowest y'
    in  (x', y', z', x'', y'')
