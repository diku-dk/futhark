def part2Indices [n] 't (conds: [n]bool) : {[n]i64 | \res-> permutationOf res (0...n-1)} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\c -> if not c then 1 else 0) conds
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  inds

-- inds_6167 = ∀i₆₁₇₆ ∈ iota n₆₀₆₈ .
--  | (conds₆₀₇₀)[i₆₁₇₆] => -1 + Σj₆₁₇₂∈[0, ..., i₆₁₇₆] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧)
--  | ¬((conds₆₀₇₀)[i₆₁₇₆]) => -1 + Σj₆₁₇₂∈[0, ..., -1 + n₆₀₆₈] (⟦(conds₆₀₇₀)[j₆₁₇₂]⟧) + Σj₆₁₇₄∈[0, ..., i₆₁₇₆] (⟦¬((conds₆₀₇₀)[j₆₁₇₄])⟧)
