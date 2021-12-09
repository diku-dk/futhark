-- Do not invent size variables for things that occur only in negative
-- position.
--
-- (This program is somewhat contrived to trigger unfortunate
-- behaviour in the type checker.)

def split_rng n rng = replicate n rng

def shuffle' rngs xs = (rngs, xs)

def main (rng: i32) (xs: []i32) =
  let rngs = split_rng (length xs) rng
  let (rngs', xs') = shuffle' rngs xs
  in xs'
