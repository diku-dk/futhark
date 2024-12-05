-- ==
-- compiled random input {[4][4][64][64]f16 [4][4][64][64]f16} auto output


-- compiled script input { (mk_input 4 4 4 64 64 64) } auto output


import "mmm-intra-helpers"

let M = 4i64
let N = 4i64
let K = 4i64

let m = 64i64
let n = 64i64
let k = 64i64

entry mk_input M N K m n k : ([M][K][m][k]f16, [K][N][k][n]f16) =
  (replicate (M * K * m * k) 1f16 |> unflatten_4d, replicate (K * N * k * n) 1f16 |> unflatten_4d)

let ne () = (replicate (m * n) 0.0f32 |> unflatten)

-- TODO try different options, degrees of sequentialization, even just return elm
def reduceOp (acc: [m][n]f32) (elm: [m][n]f32): [m][n]f32 =
--  loop acc': *[m][n]f32 = (acc : *[m][n]f32) for i < m do
--        acc' with [i, :] = map2 (+) elm[i] acc'[i]
  ne ()

--[M][N][K]

entry main (A: [M][K][m][k]f16) (B: [K][N][k][n]f16) : [M][N][m][n]f32 =
-- TODO: extract to helper used in matmul
    map (\Arow ->
        map (\Bcol ->
            map2 matmul Arow Bcol |>
--            #[incremental_flattening(only_intra)]map2 matmul Arow Bcol |>
            reduce reduceOp (ne ())
        ) (transpose B)
    ) A
