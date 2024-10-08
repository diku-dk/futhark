-- Refinement of a parametric may expand it.
-- ==
module type has_t = {type t 'a}

module type has_t' = has_twith t '^a = a