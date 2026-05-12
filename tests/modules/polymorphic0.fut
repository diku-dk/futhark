-- A simple polymorphic function in a module type.
-- ==
-- input { 1 false } output { false 1 }

module type has_identity = {val id 't : t -> t}

module with_identity : has_identity = {def id 't (x: t) = x}

def main (x: i32) (y: bool) =
  (with_identity.id y, with_identity.id x)
