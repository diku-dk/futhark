-- Without refinement, get
--   ∀i₆₁₆₂ ∈ iota n₆₀₉₀ .
--       | i₆₁₆₂ == 0 => Σj₆₁₆₀∈[0, ..., i₆₁₆₂] ((xs₆₀₉₁)[j₆₁₆₀])
--       | ¬(i₆₁₆₂ == 0) => 1337
-- which should be refined to
--   ∀i₆₁₆₂ ∈ iota n₆₀₉₀ .
--       | i₆₁₆₂ == 0 => (xs₆₀₉₁)[0]
--       | ¬(i₆₁₆₂ == 0) => 1337
-- by using the bound on i in the first case.
def sum_one_term [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i == 0 then ys[i] else 1337) iota_n
  in zs
