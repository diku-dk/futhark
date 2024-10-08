-- Refinement of a parametric type.
-- ==
-- input { 2 } output { 2 }
module type has_t = {type t 'a}

module id: (has_twith t 'a = a) = {type t 'a = a}

def main (x: i32): id.t i32 = x