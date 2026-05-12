-- From #1748

def main (a: bool) (b: bool) (c: bool) : () =
  match (a, b, c)
  case (false, false, false) -> ()
  case (false, false, true) -> ()
  case (false, true, false) -> ()
  case (_, true, true) -> ()
  case (true, _, false) -> ()
  case (true, false, true) -> ()
