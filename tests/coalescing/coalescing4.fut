-- ==
-- structure distributed { Manifest 1 }


let smoothen [n] (xs: [n]f32) =
  let pick i = unsafe xs[i32.min (n-1) (i32.max 0 i)]
  in tabulate n (\i -> pick (i-2) + pick (i-1) *4 +
                       pick i * 6 +
                       pick (i+1) * 4 + pick (i+2))

let main xss =
  xss |>
  transpose |>
  map transpose |>
  map (map smoothen) |>
  map transpose |>
  transpose
