-- this is a code snippet from bfast; it should transpose `y_error`
-- ==
-- structure distributed {Manifest 0}

let main [m][n] (nss: [m]i32) (hs: [m]i32) (y_errors: [m][n]f32) : [m]f32 =
  zip3 y_errors nss hs |>
    map (\(y_error, ns, h) -> unsafe
            map (\i -> unsafe y_error[i + ns-h+1]) (iota h)
            |> reduce (+) 0.0
        )
