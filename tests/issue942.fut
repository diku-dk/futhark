-- ==
-- input {} output { [0] }

type sometype 't = #someval t

let f (size : i32) (_ : i32) : sometype ([size]i32) =
  #someval (iota size)

let apply '^a '^b (f: a -> b) (x: a) = f x

let main : [1]i32 =
  match apply (f 1) 0
  case #someval x -> x
