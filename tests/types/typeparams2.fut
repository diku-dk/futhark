-- Shadowing in type.
-- ==
-- input { 2.0f32 } output { 2.0f32 }

type t 'int = int

def main (x: f32) : t f32 = x
