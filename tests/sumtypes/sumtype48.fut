-- ==
-- error: Unshared constructors

type t = #foo | #bar

let f b : t = if b then #foo else #baar
