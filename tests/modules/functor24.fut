module type abs = {
  type abs
}

module abs = {
  type abs = i32
}

module fieldtype (P: abs) : abs = {
  type abs = i32
}

module big_field (M: abs) = {
  type t = M.abs
}

module mod = big_field (fieldtype abs)

def main (a: mod.t) = a
