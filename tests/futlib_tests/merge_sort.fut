import "/futlib/merge_sort"
import "/futlib/math"

-- ==
-- entry: sort_i32
-- input { empty(i32) }
-- output { empty(i32) }
-- input { [5,4,3,2,1] }
-- output { [1,2,3,4,5] }
-- input { [5,4,3,3,2,1] }
-- output { [1,2,3,3,4,5] }

module i32_merge_sort = mk_merge_sort i32

entry sort_i32 (xs: []i32) = i32_merge_sort.merge_sort xs

-- ==
-- entry: sort_u16
-- input { [5u16,4u16,3u16,2u16,1u16] }
-- output { [1u16,2u16,3u16,4u16,5u16] }

module u16_merge_sort = mk_merge_sort u16

entry sort_u16 (xs: []u16) = u16_merge_sort.merge_sort xs

-- ==
-- entry: sort_f32
-- input { [5f32,4f32,3f32,2f32,1f32] }
-- output { [1f32,2f32,3f32,4f32,5f32] }

module f32_merge_sort = mk_merge_sort f32

entry sort_f32 (xs: []f32) = f32_merge_sort.merge_sort xs
