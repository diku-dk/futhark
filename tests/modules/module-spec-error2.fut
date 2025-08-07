-- ==
-- error: M1.M.t.*M0.M.t

module type MT = {
  module M: {type t val x : t val f : t -> t}
}

module M0 : MT = {
  module M = {
    type t = i32
    def x = 0
    def f (y: t) = y + 1
  }
}

module M1 : MT = M0

def main () = M1.M.f (M0.M.x)
