-- No hiding sizes behind type inference.
-- ==
-- error: do not match

let foo f x : [1]i32 =
  let r = if true then f x else [1i32]
  in r
