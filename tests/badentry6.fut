-- Fully abstract return type.
-- ==
-- warning: Entry point return

module m : { type t val x: t} = { type t = i32 let x = 0 }

let main = m.x
