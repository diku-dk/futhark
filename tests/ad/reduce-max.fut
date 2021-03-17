-- Simple reduce with multiplication
-- ==

-- compiled input { [1u32, 2, 7, 4, 5, 7, 6, 7, 3] 5u32 9u32} output { [0u32, 0, 45, 0, 0, 0, 0, 0, 0] 63f32 }

let red_max [n] (xs: [n]u32, c: u32) : u32 =
  reduce (u32.max) (0u32) xs * c

entry main [n] (xs: [n]u32) (c: u32) (d_bar: u32) =
  vjp red_max (xs,c) d_bar
