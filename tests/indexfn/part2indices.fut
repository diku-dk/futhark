-- Prelude
def sum_i64 [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def and xs =
  length xs == sum (map (\x -> if x then 1 else 0) xs)

def all 't (p: t -> bool) (xs: []t) =
  and (map (\x -> p x) xs)

def indices [n] 't (_: [n]t) =
  iota n

-- Program
def monotonic 't [n] (compare: t -> t -> bool) (xs: [n]t) =
  if n < 2 then true
  else and (map (\i -> compare xs[i] xs[i+1]) (iota (n - 1)))

def up [n] (xs: [n]i64) =
  if n < 2 then true
  else and (map (\i -> xs[i] < xs[i+1]) (iota (n - 1)))

-- NOTE iota 0 lets us write the empty array literal [].
def slice xs (a: {i64 | (>= 0)}) (b: {i64 | (<= length xs)}) =
  map (\i -> xs[a+i]) (iota (b - a))

-- TODO fix iota 0; think it causes troubles when substituted in

-- These should all pass.
def sanity_check1 n = injectiveOn (0, n) (iota n)
def sanity_check2 n = injectiveOn (0, n-1) (iota n)
def sanity_check3 n = injectiveOn (1, n) (replicate n 0)

-- TODO this fails because injectiveOn is checked in forward.
-- def permutation_of_indices [n]
--   (xs: {[n]i64 | \xs ->
--       and (map (\i -> 0 <= i && i < n) xs)
--         && injectiveOn (0, n) xs
--   }) = 1

-- def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
def part2Indices [n]
  (conds: [n]bool)
  : {(i64, [n]i64) | \(num_true, inds) ->
      num_true == sum_i64 (map (\c -> if c then 1 else 0) conds)
      && and (map (\i -> 0 <= i && i < n) inds)
      && injectiveOn (0, n) inds
    } =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  (lst, inds)

-- def lol i j = i +< j
-- def injective [n]
--   (i : {i64 | \i -> 0 <= i && i < n})
--   (j : {i64 | (+< i)})
--   (xs: [n]i64) =
--     xs[i] != xs[j]


-- def injective [n] (i: {i64 | \i -> 0 <= i && i < n}) (j: {i64 | \j -> i < j && j < n}) (xs: [n]i64) =
--   xs[i] != xs[j]

-- TODO currently not able to show this:
-- def proof_part2indices_is_injective [n]
--   -- ((i,j): {(i64, i64) | \(i,j) -> 0 <= i && i < j && j < n})
--   i (j: {i64 | \j -> 0 <= i && i < j && j < n})
--   (conds: [n]bool)
--   : {(i64, [n]i64) | \(_, inds) -> inds[i] != inds[j] }=
--   part2Indices conds




 

-- def part2Indices_ext [n] conds
--   : {(i64, [n]bool) | \(num_true, inds) ->
--       and inds
--     } =
--   let (num_true, inds) = part2Indices conds
--   in (num_true, map (\i -> if conds[i] then i < num_true else i >= num_true) inds)


-- [16]> let conds = [true,false,false,true]
-- [17]> let inds = [0,2,3i64,1]
-- [18]> scatter (replicate 4 1337) inds (map i64.bool conds)
-- [1, 1, 0, 0]
-- [19]> scatter (replicate 4 1337) inds (iota 4)
-- [0, 3, 1, 2]
