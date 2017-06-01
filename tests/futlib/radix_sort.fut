import "/futlib/radix_sort"

-- ==
-- entry: sort_i32
-- input { [5,4,3,2,1] }
-- output { [1,2,3,4,5] }
-- input { [5,4,3,3,2,1] }
-- output { [1,2,3,3,4,5] }

module i32_radix_sort = mk_radix_sort {
  type t = i32
  let num_bits = 32
  let get_bit (bit: i32) (x: i32) = (x >> bit) & 1
}

entry sort_i32 (xs: []i32) = i32_radix_sort.radix_sort xs

-- ==
-- entry: sort_u16
-- input { [5u16,4u16,3u16,2u16,1u16] }
-- output { [1u16,2u16,3u16,4u16,5u16] }


module u16_radix_sort = mk_radix_sort {
  type t = u16
  let num_bits = 16
  let get_bit (bit: i32) (x: u16) = i32((x >> u16(bit)) & 1u16)
}

entry sort_u16 (xs: []u16) = u16_radix_sort.radix_sort xs
