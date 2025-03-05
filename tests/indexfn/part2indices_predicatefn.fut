def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def part2indices [n] (p: f32 -> bool) (xs : [n]f32)
  : {[n]i64 | \inds ->
        -- We wish to prove that
        --   (1) inds is a permutation of 0, 1, ..., n - 1.
        --   (2) inds is partitioned by conds:
        --         forall i [0, n-1] . conds[i]     => inds[i] < p
        --                             not conds[i] => inds[i] >= p
        --       where p is the number of true values in conds.
        --
        let conds = map (\x -> p x) xs
        let num_true = sum (map (\c -> if c then 1 else 0) conds) 
        let step1 =
          -- A bijection from X to X is a permutation of X.
          bijectiveRCD (0, n-1) (0, n-1) inds
        let step2 =
          and (map2 (\c ind -> if c then ind < num_true else ind >= num_true) conds inds)
        in step1 && step2
    } =
  let conds = map (\x -> p x) xs
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  inds
