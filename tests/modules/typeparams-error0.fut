-- ==
-- error: vector

module type MT = {
  type vector 'a
}

module M : MT = {type vector = []i32}
