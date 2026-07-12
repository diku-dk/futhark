-- ==
-- entry: calculate_dangling_ranks
-- random input { [12]f32 [12]i32}
-- output { true }

def calculate_dangling_ranks_orig [n] (ranks: [n]f32) (sizes: [n]i32): *[]f32 =
  let zipped = zip sizes ranks
  let weights = map (\(size, rank) -> if size == 0 then rank else 0f32) zipped
  let total = f32.sum weights / f32.i64 n
  in map (+total) ranks

def calculate_dangling_ranks_am [n] (ranks: [n]f32) (sizes: [n]i32): *[]f32 =
  let weights = f32.bool (sizes == 0) * ranks
  let total = f32.sum weights / f32.i64 n
  in ranks + total

entry calculate_dangling_ranks [n] (ranks: [n]f32) (sizes: [n]i32): bool =
  and (calculate_dangling_ranks_orig ranks sizes == calculate_dangling_ranks_am ranks sizes)
