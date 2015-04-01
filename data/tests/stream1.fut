fun {{int,[int]},[[int]],[real],[real],[int]} main(int m, *[int,n] A) =
  let B = map(+10, A) in
  stream( fn {{int,[int]},[[int,!m]],[real],[real],[int]} ({int, [int,!m]} acc2, *[int] C) =>
                    let {acc0, acc} = acc2                in
                    let X = map ( fn [int] (int c) => 
                                    map(+c, iota(m))
                                , C )                     in
                    let Y0= scan( fn [int] ([int] acc, [int] x) =>
                                    zipWith(+, acc, x)
                                , replicate(m,0), X )     in
                    let Y = map ( fn [int] ([int] y0) => 
                                    zipWith(+, acc, y0) 
                                , Y0 )                    in
                    let Z = map ( fn real ([int] y) => 
                                    let rs = map( fn real (int u) => 
                                                    toReal(3*u)
                                                , y ) 
                                    in  reduce(+, 0.0, rs )
                                , Y )                     in
                    let W = filter( fn bool (real z) => 
                                        (z / 55.0) > 4.0
                                  , Z )                   in
//                    let D = scan (+, 0, C)                in
//                    let E = map (+acc0, D)                in
                    // C becomes scan + 0 C
                    let C[0] = C[0] + acc0                in
                    loop (C) = 
                        for j < chunk-1 do
                            let C[j+1] = C[j+1] + C[j]
                            in  C
                    in
                    { {C[chunk-1],Y[chunk-1]}, Y, Z, W, C }
        , chunk, i, {0,replicate(m,0)}, copy(B)
        )
