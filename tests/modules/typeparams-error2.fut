-- Type parameters may not be duplicated.
-- ==
-- error: previously

module type mt = {
  type whatevs 't 't
}
