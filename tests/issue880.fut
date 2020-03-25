-- ==
-- error: Attempt to consume variable "xs"

type t = {xs: [10]i32}

let f ({xs}: t) : t = {xs = xs}

let g (s: t) =
  let s = f s
  let s = loop s = f s for _i < 10 do f s
  in s

let main xs =
  let {xs} = g {xs}
  in xs with [0] = 0
