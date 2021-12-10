-- Fully abstract return type.
-- ==
-- warning: Entry point return

module m : { type t val x: t} = { type t = i32 def x = 0 }

def main = m.x
