-- Fully abstract parameter type.
-- ==
-- warning: Entry point parameter

module m : { type t val x: t} = { type t = i32 def x = 0 }

def main (x: m.t) = 0i32
