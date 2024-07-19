-- Enums in simple modules.
-- ==
-- input { }
-- output { 3 }

module enum_module = {
  type foobar = #foo | #bar
  def f (x : foobar) : i32 =
    match x
      case #foo -> 1
      case #bar -> 2
  def foo = #foo : foobar

}

def main : i32 = match (enum_module.f (#foo : enum_module.foobar))
                  case 1 -> 3
                  case 2 -> 4
                  case _ -> 5
