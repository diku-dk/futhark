-- Local functions should not affect aliasing.

def main (ops: []i32) (exs: []i32) =
  let correct op ex = op == ex
  in ops |> filter (\op -> all (correct op) exs)
