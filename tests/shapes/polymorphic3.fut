-- We must be able to infer size-preserving function types.

let set i v arr =
  copy arr with [i] = v

let main [n] (xs: [n]i32) : [n]i32 =
  set 0 0 xs
