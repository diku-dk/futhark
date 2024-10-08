-- Refinement of a parametric with a non-parametric type is not OK.
-- ==
-- error: Cannot refine a type
module type has_t = {type t 'a}

module type has_t' = has_twith t  = i32