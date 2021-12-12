-- Arrays literals of sum types.
-- ==
-- error: bool

type t = #c i32

def main =
  let ts = [#c 1, #c false] : []t
  in 0i32
