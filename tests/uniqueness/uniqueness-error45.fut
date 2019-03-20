-- A local function must not return a global array.
-- ==
-- error: aliases the free variable `global`

let global: []i32 = [1,2,3]

let main =
  let f (b: bool) = if b then global else []
  in f
