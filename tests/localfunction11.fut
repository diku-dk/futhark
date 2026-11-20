-- Local functions should not affect aliasing.
-- ==
-- tags { no_webgpu }

def main (ops: []i32) (exs: []i32) =
  let correct op ex = op == ex
  in ops |> filter (\op -> all (correct op) exs)
