fun {int,[real]} main(*[int] A) = 
  stream( fn {int,*[real]} (int acc, *[int] a) =>
                    let x = map (+4,   a ) in 
                    let y0= scan(+, 0, x ) in
                    let y = map (+acc, y0) in
                    let z = map ( fn real (int a) => toReal(3*a), y) in
                    let u = y0[chunk-1] 
                    in { acc+u, copy(z) }
        , chunk, i, 0, A
        )
