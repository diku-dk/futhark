-- Does 'with' work?
-- ==
-- input { 2 } output { 42 }
module type constant = {type t val x: t}

module intconstant: (constantwith t  = i32) = {
  type t = i32
  
  def x = 40
}

def main (y: i32) = intconstant.x + y