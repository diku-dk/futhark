-- Fully abstract parameter type.
-- ==
-- warning: Entry point parameter

module m : { type t val x: t} = { type t = i32 let x = 0 }

let main (x: m.t) = 0i32
