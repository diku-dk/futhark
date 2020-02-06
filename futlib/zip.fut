-- | Transforming arrays of tuples into tuples of arrays and back
-- again.  These are generally very cheap operations, as the internal
-- compiler representation is always tuples of arrays.

-- The main reason this module exists is that we need it to define
-- SOACs like `map2`@term@"/futlib/soacs".

-- We need a map to define some of the zip variants, but this file is
-- depended upon by soacs.fut.  So we just define a quick-and-dirty
-- internal one here that uses the intrinsic version.
local let internal_map 'a [n] 'x (f: a -> x) (as: [n]a): [n]x =
  intrinsics.map (f, as) :> [n]x

-- | Construct an array of pairs from two arrays.
let zip [n] 'a 'b (as: [n]a) (bs: [n]b): [n](a,b) =
  intrinsics.zip (as, bs) :> [n](a,b)

-- | Construct an array of pairs from two arrays.
let zip2 [n] 'a 'b (as: [n]a) (bs: [n]b): [n](a,b) =
  zip as bs :> [n](a,b)

-- | As `zip2`@term, but with one more array.
let zip3 [n] 'a 'b 'c (as: [n]a) (bs: [n]b) (cs: [n]c): [n](a,b,c) =
  internal_map (\(a,(b,c)) -> (a,b,c)) (zip as (zip2 bs cs))

-- | As `zip3`@term, but with one more array.
let zip4 [n] 'a 'b 'c 'd (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d): [n](a,b,c,d) =
  internal_map (\(a,(b,c,d)) -> (a,b,c,d)) (zip as (zip3 bs cs ds))

-- | As `zip4`@term, but with one more array.
let zip5 [n] 'a 'b 'c 'd 'e (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d) (es: [n]e): [n](a,b,c,d,e) =
  internal_map (\(a,(b,c,d,e)) -> (a,b,c,d,e)) (zip as (zip4 bs cs ds es))

-- | Turn an array of pairs into two arrays.
let unzip [n] 'a 'b (xs: [n](a,b)): ([n]a, [n]b) =
  intrinsics.unzip xs :> ([n]a, [n]b)

-- | Turn an array of pairs into two arrays.
let unzip2 [n] 'a 'b (xs: [n](a,b)): ([n]a, [n]b) =
  unzip xs

-- | As `unzip2`@term, but with one more array.
let unzip3 [n] 'a 'b 'c (xs: [n](a,b,c)): ([n]a, [n]b, [n]c) =
  let (as, bcs) = unzip (internal_map (\(a,b,c) -> (a,(b,c))) xs)
  let (bs, cs) = unzip bcs
  in (as, bs, cs)

-- | As `unzip3`@term, but with one more array.
let unzip4 [n] 'a 'b 'c 'd (xs: [n](a,b,c,d)): ([n]a, [n]b, [n]c, [n]d) =
  let (as, bs, cds) = unzip3 (internal_map (\(a,b,c,d) -> (a,b,(c,d))) xs)
  let (cs, ds) = unzip cds
  in (as, bs, cs, ds)

-- | As `unzip4`@term, but with one more array.
let unzip5 [n] 'a 'b 'c 'd 'e (xs: [n](a,b,c,d,e)): ([n]a, [n]b, [n]c, [n]d, [n]e) =
  let (as, bs, cs, des) = unzip4 (internal_map (\(a,b,c,d,e) -> (a,b,c,(d,e))) xs)
  let (ds, es) = unzip des
  in (as, bs, cs, ds, es)
