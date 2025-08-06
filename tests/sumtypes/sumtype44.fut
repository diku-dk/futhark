-- ==
-- error: Unmatched

type inst =
    #foo
  | #bar
  | #baz

def exec (inst: inst) =
  #[unsafe]
  let x = 0i32
  in match inst
     case #foo ->
       x + 1
     case #baz ->
       x - 1
