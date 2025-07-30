-- ==
-- error: cannot unify type with constructors

type found = #found i32 | #not_found

def main =
  let o = map (\x -> if (x > 3) then (#found x) else (#not_found)) [0, 1, 2, 3, 4]
  let u =
    match o
    case #found x -> x
    case #not_found -> -1
  in u
