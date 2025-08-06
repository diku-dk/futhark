-- Yet another case of aliasing that can result in incorrect code
-- generation.

def main (w: i64) (h: i64) =
  ([1, 2, 3] :> [w * h]i32) |> unflatten
