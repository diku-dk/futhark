-- Sumtype in-place updates.
-- ==
-- input { }
-- output { [-1, -2, -3, -4, 2, 4, 6, 8] }

type mooboo = #moo i32 | #boo i32

let swap_inplace (ns : []i32) : *[]mooboo =
  let x = map (\n -> #moo n) ns ++ map (\n -> #boo n) ns
  in loop x for i < 2*(length ns) do
      x with [i] = match x[i]
                   case (#moo x) -> #boo (-x)
                   case (#boo x) -> #moo (2 * x)

let f (x : mooboo) : i32 =
  match x
    case (#moo x) -> x
    case (#boo x) -> x

let main : []i32 = map f (swap_inplace [1,2,3,4])
