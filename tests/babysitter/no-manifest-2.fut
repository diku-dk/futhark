-- this is a code snippet from bfast; it should transpose `y_error`
-- ==
-- structure gpu {Manifest 0}

def main [m] [n] (nss: [m]i64) (hs: [m]i64) (y_errors: [m][n]f32) : [m]f32 =
  zip3 y_errors nss hs
  |> map (\(y_error, ns, h) ->
            map (\i -> y_error[i + ns - h + 1]) (iota h)
            |> reduce (+) 0.0)
