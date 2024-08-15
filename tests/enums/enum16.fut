-- Enum in-place updates.
-- ==
-- input { }
-- output { [2, 2, 1, 1] }

def swap_inplace (n : i64) : *[]#foo | #bar =
  let x = replicate n #foo ++ replicate n #bar
  in loop x for i < 2*n do
      x with [i] = match x[i]
                   case #foo -> #bar
                   case #bar -> #foo

def f (x : #foo | #bar) : i32 =
  match x
    case #foo -> 1
    case #bar -> 2

def main : []i32 = map f (swap_inplace 2)
