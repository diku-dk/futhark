-- Refinement of a parametric type must not narrow it.  This rule may
-- be too restrictive and we may loosen it in the future.
-- ==
-- error: Cannot refine a type

module type has_t = {type t '^a}
module type has_t' = has_t with t 'a = a
