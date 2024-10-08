-- Refinement of a non-parametric with a parametric type is not OK.
-- ==
-- error: Cannot refine a type
module type has_t = {type t}

module type has_t' = has_twith t 'a = a