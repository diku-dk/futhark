-- With of a type in an inner module.
-- ==
-- error: module type requires
module type has_t = {type t}

module type has_inner = {module inner: has_t}

module m: has_innerwith inner.t  = bool = {module inner = {type t = i32}}

def main (): m.inner.t = 2