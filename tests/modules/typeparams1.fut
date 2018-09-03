-- Use of a parametric type inside a module type.
-- ==
-- input {} output {2}

module type MT = {
  type vector 'a
  type i32matrix = [](vector i32)
}

module M0: MT = {
  type vector 'a = []a
  type i32matrix = [](vector i32)
}

-- And now an inlined one.
module M1: MT = {
  type vector 'a = []a
  type i32matrix = [][]i32
}

let main = 2
