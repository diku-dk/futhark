-- Tiling irregular parallelism inside of a loop.  It is incredible
-- that this generates working code.
-- ==
-- random input { [1000][3][3]f32 }
-- structure gpu { SegMap/DoLoop/DoLoop/SegMap 2 }

def argmax (arr: []f32) =
  reduce_comm (\(a,i) (b,j) ->
                 if a < b
                 then (b,j)
                 else if b < a then (a,i)
                 else if j < i then (b, j)
                 else (a, i))
              (0, 0)
              (zip arr (indices arr))

def f [m] [n] (A:[m][n]f32) =
  loop A for i < i64.min m n do
  let j = A[i:,i] |> map f32.abs |> argmax |> (.1) |> (+i)
  in map (map (*A[j,j])) A

def main ms = #[sequential_inner] map f ms
