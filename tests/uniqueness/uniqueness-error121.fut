-- A type parameter may not occur at a negative position other than its
-- sources, including in the parameter of a source: "g" is handed values of "b"
-- by the function it passes to "f".  The result of "g" is therefore not
-- refined, and so aliases what "f" captures.
-- ==
-- error: consumed

def g 'b (f: (b -> i32) -> b) : b =
  f (\_ -> 0)

def main (x: i32) =
  let zs = replicate 3 x
  let f = \(_: [3]i32 -> i32) : *[3]i32 -> map (+ 1) zs
  let r = g f
  let r[0] = 0
  in (r, zs)
