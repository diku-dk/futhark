-- Matrix multiplication written imperatively.  Very slow when using
-- GPU backends.
--
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
fun *[[int]] matmultImp([[int]] a, [[int]] b) =
    let N   = size(0, a)            in
    let res = replicate(N, iota(N)) in
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
