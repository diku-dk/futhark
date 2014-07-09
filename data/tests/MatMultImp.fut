fun *[[int]] matmultImp([[int]] a, [[int]] b) =
    let N   = size(0, a)            in
    let res = copy(replicate(N, iota(N))) in
    loop (res) = for i < N do
        loop (res) = for j < N do
            let partsum =
                let res = 0 in
                loop (res) = for k < N do
                    let res = res + a[i,k] * b[k,j]
                    in  res
                in res
            in let res[i,j] = partsum in res
        in res
    in res


fun [[int]] main([[int]] x, [[int]] y) =
  matmultImp(x, y)
