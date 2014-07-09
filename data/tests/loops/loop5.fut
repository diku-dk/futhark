fun [int] main() =
    let N = 10 in
    let x = copy(iota(N))   in
    loop (x) =
        for i < N-1 do
            let x[i+1] = x[i+1] + x[i]
            in  x
    in x
