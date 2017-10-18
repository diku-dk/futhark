-- A record value must have at least the fields of its corresponding
-- type.
-- ==
-- error: cannot match

let main() =
  let r:{a:i32,b:i32} = {a=0}
  in 0