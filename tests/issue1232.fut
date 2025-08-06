type option 'a = #some a | #none

def bind 'a 'b (m: option a) (f: a -> option b) : option b =
  match m
  case #none -> #none
  case #some a -> f a

entry foo (n: i32) : bool =
  match bind (if n == 0 then #some () else #none)
             (\() -> #some true)
  case #some res -> res
  case #none -> true
