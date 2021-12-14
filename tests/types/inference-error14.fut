-- A record must have an unambiguous type (no row polymorphism).
-- ==
-- error: ambiguous

def f x = x.l
