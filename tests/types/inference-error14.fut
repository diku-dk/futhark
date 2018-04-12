-- A record must have an unambiguous type (no row polymorphism).
-- ==
-- error: ambiguous

let f x = x.l
