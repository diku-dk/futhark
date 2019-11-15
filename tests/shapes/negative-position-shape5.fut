-- Do not invent size variables for things that occur only in negative
-- position.
--
-- (This program is somewhat contrived to trigger unfortunate
-- behaviour in the type checker.)

let split_rng n rng = replicate n rng

let shuffle' rngs xs = (rngs, xs)

let main (rng: i32) (xs: []i32) =
  let rngs = split_rng (length xs) rng
  let (rngs', xs') = shuffle' rngs xs
  in xs'
