import "/futlib/radix_sort"

-- ==
-- entry: sort_i32
-- input { [5,4,3,2,1] }
-- output { [1,2,3,4,5] }
-- input { [5,4,3,3,2,1] }
-- output { [1,2,3,3,4,5] }

entry sort_i32 = radix_sort i32.num_bits i32.get_bit

-- ==
-- entry: sort_u16
-- input { [5u16,4u16,3u16,2u16,1u16] }
-- output { [1u16,2u16,3u16,4u16,5u16] }

entry sort_u16 = radix_sort u16.num_bits u16.get_bit

-- ==
-- entry: sort_f32
-- input { [5f32,4f32,3f32,2f32,1f32] }
-- output { [1f32,2f32,3f32,4f32,5f32] }

entry sort_f32 = radix_sort f32.num_bits f32.get_bit
