-- A shape parameter cannot be used before it has been in positive
-- position at least once. This program would be okay if the
-- parameters were swapped.
-- ==
-- error: Shape parameter `n` must first be given .*

let f [n] (g: i32 -> [n]i32) (xs: [n]i32) =
  let g' (x: i32) = g x : [n]i32
  in (g' (length xs), n)
