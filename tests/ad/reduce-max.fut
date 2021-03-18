-- Simple reduce with multiplication
-- ==
-- compiled input { [1u32, 2u32, 7u32, 4u32, 5u32, 7u32, 6u32, 7u32, 3u32] 5u32 9u32}
-- output { [0u32, 0u32, 45u32, 0u32, 0u32, 0u32, 0u32, 0u32, 0u32] 63u32 }
-- compiled input { [1u32, 2u32, 5u32, 4u32, 5u32, 3u32, 2u32, 1u32, 3u32] 5u32 9u32}
-- output { [0u32, 0u32,  0u32, 0u32, 0u32, 0u32, 0u32, 0u32, 0u32] 54u32 }

let red_max [n] (xs: [n]u32, c: u32) : u32 =
  reduce (u32.max) (6u32) xs * c

entry main [n] (xs: [n]u32) (c: u32) (d_bar: u32) =
  vjp red_max (xs,c) d_bar