-- With of a type in an inner module.
-- ==
-- input {} output {2}

module type has_t = { type t }

module type has_inner = { module inner: has_t }

module m: has_inner with inner.t = i32 = { module inner = { type t = i32 } }

def main: m.inner.t = 2
