-- Prelude
def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def slice xs (a: {i64 | (>= 0)}) (b: {i64 | (<= length xs)}) =
  map (\i -> xs[a+i]) (iota (b - a))


-- These should all pass.
def sanity_check1 n = injectiveOn (0, n) (iota n)
def sanity_check2 n = injectiveOn (0, n-1) (iota n)
def sanity_check3 n = injectiveOn (1, n) (replicate n 0)

def part2indices [n]
  (conds: [n]bool)
  : {(i64, [n]i64) | \(num_true, inds) ->
        num_true <= n
        -- Proof that inds are a permutation of 0 ... n - 1.
        && and (map (\i -> 0 <= i && i < n) inds)
        && injectiveOn (0, n-1) inds
        -- Proof that inds partition according to conds.
        && num_true == sum (map (\c -> if c then 1 else 0) conds)
        && and (map2 (\c ind ->
                       if c
                       then ind < num_true
                       else ind >= num_true
                     ) conds inds)
    } =
  -- We could also do the refinements on each element of the tuple.
  -- : ({i64 | \n ->
  --       n == sum (map (\c -> if c then 1 else 0) conds)
  --    }, {[n]i64 | \inds ->
  --     and (map (\i -> 0 <= i && i < n) inds)
  --       && injective inds
  --   }) =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  (lst, inds)
