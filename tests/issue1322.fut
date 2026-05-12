def resolve_fns [n] (a: [n]bool) (data: [n]i32) =
  let b = map (== 0) data
  let d =
    b
    |> map i32.bool
    |> scan (+) 0
    |> map (+ -1)
  let is = data |> map i64.i32
  let f = scatter (replicate n (-1i32)) is d
  let g =
    f
    |> map (!= -1)
    |> reduce (&&) true
  let new_data = scatter (copy data) is d
  in (g, new_data)

def resolve_vars [n] (a: [n]bool) =
  let valid = reduce (&&) true a
  let data = map i32.bool a
  in (valid, data)

entry oef [n] (a: [n]bool) =
  let (b, data) = resolve_vars a
  let (c, data) = resolve_fns a data
  in (b && c, data)
