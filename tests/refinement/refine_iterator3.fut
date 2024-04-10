-- This should _not_ touch the sum. Want:
--   ∀i₆₁₇₂ ∈ iota n₆₁₂₇ .
--     | i₆₁₇₂ < 0 => Σj₆₁₇₀∈[0, ..., i₆₁₇₂] ((xs₆₁₂₈)[j₆₁₇₀])
--     | ¬(i₆₁₇₂ < 0) => i₆₁₇₂
-- TODO should a neutral element be inserted here?
def sum_empty [n] (xs: [n]i64) : {[n]i64 | \res-> permutationOf res (0...6)} =
  let ys = scan (+) 0 xs
  let iota_n = iota n
  let zs = map (\i -> if i < 0 then ys[i] else i) iota_n
  in zs
