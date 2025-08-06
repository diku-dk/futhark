-- SGEMM  performs the matrix-matrix operation:
--    C := alpha * A * B + beta * C
-- ==
-- entry: main1 main2
-- no_python compiled random input { [1024][1024]f32 [1024][1024]f32 [1024][1024]f32 0.5f32 0.75f32} auto output

entry main1 [n] [m] [q]
            (A: [n][q]f32)
            (B: [q][m]f32)
            (C: [n][m]f32)
            (alpha: f32)
            (beta: f32) : [n][m]f32 =
  map2 (\Arow Crow ->
          map2 (\Bcol c ->
                  let x = map2 (*) Arow Bcol |> f32.sum
                  in alpha * x + beta * c)
               (transpose B)
               Crow)
       A
       C

entry main2 [n] [m] [q]
            (A: [n][q]f32)
            (B: [q][m]f32)
            (C: [n][m]f32)
            (alpha: f32)
            (beta: f32) : [n][m]f32 =
  map2 (\Arow i ->
          map2 (\Bcol j ->
                  let y = beta * #[unsafe] C[i / 2, j / 2 + 1]
                  let x = map2 (*) Bcol Arow |> f32.sum
                  in alpha * x + y)
               (transpose B)
               (iota m))
       A
       (iota n)

-- ==
-- entry: main3
-- compiled random input { [1024][1024]f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32 0.5f32 0.75f32} auto output

entry main3 [n] [m] [q]
            (A: [n][q]f32)
            (B: [q][m]f32)
            (C: [n][m]f32)
            (D: [n][q]f32)
            (alpha: f32)
            (beta: f32) : [n][m]f32 =
  map3 (\Arow Drow Crow ->
          map2 (\Bcol c ->
                  let x = map3 (\a d b -> a * d * b) Arow Drow Bcol |> f32.sum
                  in alpha * x + beta * c)
               (transpose B)
               Crow)
       A
       D
       C
