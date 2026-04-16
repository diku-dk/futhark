-- | ignore

import "merge_sort"

-- picking small block sizes should stress all codepaths within
-- merge_sort, even on small inputs
def params = {max_block_size = 2i64, max_merge_block_size = 2i64}

-- ==
-- entry: sort_i32
-- input { empty([0]i32) }
-- output { empty([0]i32) }
-- input { [5,4,3,2,1] }
-- output { [1,2,3,4,5] }
-- input { [5,4,3,3,2,1] }
-- output { [1,2,3,3,4,5] }

entry sort_i32 (xs: []i32) =
  merge_sort_with_params params (i32.<=) xs

-- ==
-- entry: sort_u16
-- input { [5u16,4u16,3u16,2u16,1u16] }
-- output { [1u16,2u16,3u16,4u16,5u16] }

entry sort_u16 (xs: []u16) =
  merge_sort_with_params params (u16.<=) xs

-- ==
-- entry: sort_f32
-- input { [5f32,4f32,3f32,2f32,1f32] }
-- output { [1f32,2f32,3f32,4f32,5f32] }

entry sort_f32 (xs: []f32) =
  merge_sort_with_params params (f32.<=) xs

-- ==
-- entry: sort_perm_i32
-- input { [5,4,3,2,1,0,-1,-2] }
-- output { [7, 6, 5, 4, 3, 2, 1, 0] }

entry sort_perm_i32 [n] (xs: [n]i32) =
  zip xs (iota n)
  |> merge_sort_with_params_by_key params (.0) (<=)
  |> map ((.1) >-> i32.i64)

-- ==
-- entry: sort_perm_f32
-- input { [5f32,4f32,3f32,2f32,1f32,0f32,-1f32,-2f32] }
-- output { [7, 6, 5, 4, 3, 2, 1, 0] }

entry sort_perm_f32 [n] (xs: [n]f32) =
  zip xs (iota n)
  |> merge_sort_with_params_by_key params (.0) (<=)
  |> map ((.1) >-> i32.i64)
