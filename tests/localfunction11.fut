-- Local functions should not affect aliasing.

let main (ops: []i32) (exs: []i32) =
  let correct op ex = op == ex
  in ops |> filter (\op -> all (correct op) exs)
