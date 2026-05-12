-- Looking at the size of an existential pattern match.
-- ==
-- input {  true 1i64 2i64 } output { 1i64 }
-- input { false 1i64 2i64 } output { 2i64 }

def main b n m =
  let arr =
    match b
    case true -> iota n
    case false -> iota m
  in length arr
