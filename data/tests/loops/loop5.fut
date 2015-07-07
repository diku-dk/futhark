-- ==
-- input {
-- }
-- output {
--   [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
-- }
fun [int] main() =
    let N = 10 in
    let x = copy(iota(N))   in
    loop (x) =
        for i < N-1 do
            let x[i+1] = x[i+1] + x[i]
            in  x
    in x
