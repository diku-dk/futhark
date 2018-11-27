-- Enums in module types.
-- ==
-- input { }
-- output { 3 }

module type foobar_mod = {
  type foobar
  val f : foobar -> i32
  val foo : foobar
}

module enum_module : foobar_mod = {
  type foobar = #foo | #bar
  let f (x : foobar) : i32 =
    match x
      case #foo -> 1 
      case #bar -> 2
  let foo = #foo : foobar

}

let main : i32 = match (enum_module.f enum_module.foo)
                  case 1 -> 3
                  case 2 -> 4
                  case _ -> 5
