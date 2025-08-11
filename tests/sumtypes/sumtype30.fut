-- Deduplication for nested sum types.

type either 'a 'b = #left a | #right b

type t = either bool (either (either i32 i32) i32)

def main (x: i32) =
  match (#right (#left (#left x))) : t
  case #right (#right x) -> x - 1
  case #right (#left (#left x)) -> x
  case _ -> 0
