-- A mismatched module spec.
-- ==
-- error: QUUX

module type MT = {
  module M: {val QUUX : i32}
}

module M1 : MT = {
  module M = {def QUUX2 = 2}
}

def main () = M1.M.x
