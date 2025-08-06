-- Multiple abstract types with the same unqualified name.  This will
-- not work if the type checker simply flattens out all abstract
-- types.
-- ==
-- input { } output { 2 }

module type MT = {
  module A: {type a type b}
  module B: {type a type b}
}

module M : MT = {
  module A = {type a = i32 type b = bool}
  module B = {type a = bool type b = i32}
}

def main = 2
