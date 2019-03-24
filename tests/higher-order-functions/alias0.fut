-- Yet another case of aliasing that can result in incorrect code
-- generation.

let main (w: i32) (h: i32) =
  [1,2,3] |> unflatten w h
