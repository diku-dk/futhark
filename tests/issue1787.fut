-- ==
-- error: function type

entry main: i32 -> i32 -> i32 =
  ((true, (.0)), (false, (.1)))
  |> (\p -> if p.0.0 then p.0 else p.1)
  |> (.1)
  |> curry
