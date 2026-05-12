-- Polymorphic function called incorrectly.
-- ==
-- error: Cannot apply "f"

def f 't (x: t) (y: t) = (x, y)

def main () = f 1 false
