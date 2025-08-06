-- A parametric module does not match a module spec.
-- ==
-- error: parametric

module type MT = {
  module M: {val x : i32}
}

module M1 : MT = {
  module M (P: {val y : i32}) = {def x = P.y}
}

def main () = M1.M.x
