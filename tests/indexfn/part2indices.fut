-- Prelude
def sum_i64 [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def slice xs (a: {i64 | (>= 0)}) (b: {i64 | (<= length xs)}) =
  map (\i -> xs[a+i]) (iota (b - a))


-- These should all pass.
def sanity_check1 n = injectiveOn (0, n) (iota n)
def sanity_check2 n = injectiveOn (0, n-1) (iota n)
def sanity_check3 n = injectiveOn (1, n) (replicate n 0)

-- [NOTE] So far:
-- - andR makes proofs easier because futhark `and` is problematic in that
--   we have to substitute index functions (with multiple cases)
--   into a sum!
--   (This only fails when the andR check would fail anyway, however.
--    Since, if the check succeeds, all cases are 1 and hence get merged.
--    But it also makes it impossible to use andR outside refinements:
--      def injective xs = andR (map (\i -> xs[i] != xs[i+1]) (iota (n - 1))).)
--
-- - andR injectivity proof doesn't go through, but the simplifications
--   needed are obvious:
--      forall i₁₉₃₁₂ . | (conds₄₆₉₂[i₁₉₃₁₂]) ^ (conds₄₆₉₂[1 + i₁₉₃₁₂]) ⇒    1
--                      | (¬(conds₄₆₉₂[i₁₉₃₁₂])) ^ (conds₄₆₉₂[1 + i₁₉₃₁₂]) ⇒
--                        1 + i₁₉₃₁₂ + ∑j₂₇₀₈₇∈(2 + i₁₉₃₁₂ .. -1 + n₄₆₉₁) (conds₄₆₉₂[j₂₇₀₈₇])
--                          ≠ ∑j₂₇₀₈₇∈(0 .. i₁₉₃₁₂) (conds₄₆₉₂[j₂₇₀₈₇])
--                      | (conds₄₆₉₂[i₁₉₃₁₂]) ^ (¬(conds₄₆₉₂[1 + i₁₉₃₁₂])) ⇒    1
--                      | (¬(conds₄₆₉₂[i₁₉₃₁₂])) ^ (¬(conds₄₆₉₂[1 + i₁₉₃₁₂])) ⇒
--                        i₁₉₃₁₂ + ∑j₂₇₀₈₇∈(1 + i₁₉₃₁₂ .. -1 + n₄₆₉₁) (conds₄₆₉₂[j₂₇₀₈₇])
--                          ≠ 1 + i₁₉₃₁₂ + ∑j₂₇₀₈₇∈(2 + i₁₉₃₁₂ .. -1 + n₄₆₉₁) (conds₄₆₉₂[j₂₇₀₈₇]) ]
--   Add tests for Cosmin: [x]
--   Done? [ ]
--
-- - Once andR goes through, I actually think that plain and might go through
--   as well...
-- 
-- - injectiveOn/injective works
-- 
-- - andR and injectiveOn in forward creates the problem that you cannot
--   put this on preconditions in a top-level def, because it will try
--   to show the precondition on the formal parameter during forward
--   Solution: Delay actual checking to pre- and postcondition checks.
--   Done? [ ]
-- 
-- - TODO fix iota 0; think it causes troubles when substituted in
-- 
def part2Indices [n]
  (conds: [n]bool)
  : ({i64 | \n ->
        n == sum_i64 (map (\c -> if c then 1 else 0) conds)
     }, {[n]i64 | \inds ->
      and (map (\i -> 0 <= i && i < n) inds)
      && injective inds
      -- && injectiveOn (0, n) inds
      -- && and (map (\i -> inds[i] != inds[i+1]) (iota (n-1)))
      -- && and (map2 (\x y -> x != y) (sized (n-1) (slice inds 0 (n-1))) (slice inds 1 n))
    }) =
  -- We can also do it tupled if the checks depend on each other:
  -- : {(i64, [n]i64) | \(num_true, inds) ->
  --     ..
  --   } =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  (lst, inds)

-- TODO this fails because injectiveOn is checked in forward.
-- def permutation_of_indices [n]
--   (xs: {[n]i64 | \xs ->
--       and (map (\i -> 0 <= i && i < n) xs)
--         && injectiveOn (0, n) xs
--   }) = xs
-- def injective [n] (xs: [n]i64) =
--   and (map (\i -> xs[i] != xs[i+1]) (iota (n - 1)))
-- def injective [n] (xs: [n]i64) =
--   injectiveOn (0, n) xs

-- [Fun stuff]
--
-- def and xs =
--   length xs == sum (map (\x -> if x then 1 else 0) xs)

-- def all 't (p: t -> bool) (xs: []t) =
--   and (map (\x -> p x) xs)

-- def monotonic 't [n] (compare: t -> t -> bool) (xs: [n]t) =
--   if n < 2 then true
--   else and (map (\i -> compare xs[i] xs[i+1]) (iota (n - 1)))

-- def up [n] (xs: [n]i64) =
--   if n < 2 then true
--   else and (map (\i -> xs[i] < xs[i+1]) (iota (n - 1)))

