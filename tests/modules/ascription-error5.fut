-- Check that type mismatches in nested modules use qualified names.
-- ==
-- error: bar.t

module type mt = {
  module foo: {
    type t = i32
  }

  module bar: {
    type t = bool
  }
}

module m : mt = {
  module foo = {type t = i32}
  module bar = {type t = i32}
}
